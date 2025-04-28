(ns generic-procedure.procedures
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [generic-procedure.collection :as collection]
            ))

;; ------------------------------
;; Environment and Package Management
;; ------------------------------
(def my-env (atom {}))
(defn install-package! [package-name bindings]
  (swap! my-env assoc package-name bindings))

;; ------------------------------
;; Arithmetic and Generic Procedure System
;; ------------------------------
(defrecord Arithmetic [name bases domain-predicate constant-map operation-map])

(defn arithmetic-constant-names-for [bases]
  (let [consts (apply set/union (map #(keys (:constant-map %)) bases))]
    (if (seq consts)
      consts
      #{'pi 'e 'modulus})))

(defn arithmetic-constants-for [name bases]
  (remove nil? (map #(get-in % [:constant-map name]) bases)))

(defn add [x y] (+ x y))
(defn sub [x y] (- x y))
(defn mul [x y] (* x y))
(defn div [x y] (/ x y))

(defn evaluate [result]
  (let [func (first result)
        args (rest result)]
    (apply func args)))

(def system-global-environment
  (atom
   {:add add, :sub sub, :mul mul, :div div}))

(def primitive-operation-map [:add :sub :mul :div])

(defn arithmetic-operators-for [bases]
  (let [ops (apply set/union (map #(keys (:operation-map %)) bases))]
    (if (seq ops)
      ops
      primitive-operation-map)))

(defn arithmetic-operations-for [operator bases]
  (keep #(get-in % [:operation-map operator]) bases))

(defn arithmetic-operators [arithmetic]
  (keys (or (:operation-map arithmetic) {})))

(defn arithmetic-operation [operator arithmetic]
  (get-in arithmetic [:operation-map operator]))

(defn arithmetic-procedure [operator arithmetic]
  (arithmetic-operation operator arithmetic))

(defn make-arithmetic [name domain-predicate bases get-constant get-operation]
  (let [inherited-constants (apply merge (map :constant-map bases))
        inherited-operations (apply merge (map :operation-map bases))
        operators (arithmetic-operators-for bases)
        constant-map (merge inherited-constants
                            (into {}
                                  (keep (fn [c]
                                          (let [base-constants (arithmetic-constants-for c bases)]
                                            (when (= (count bases) (count base-constants))
                                              [c (apply get-constant c base-constants)]))))
                                  (arithmetic-constant-names-for bases)))
        operation-map (merge inherited-operations
                             (into {}
                                   (keep (fn [op]
                                           (let [base-operations (arithmetic-operations-for op bases)]
                                             (if (seq base-operations)
                                               [op (apply get-operation op base-operations)]
                                               [op (get-operation op)]))))
                                   operators))]
    (->Arithmetic name bases domain-predicate constant-map operation-map)))

(defonce current-arithmetic (atom nil))

(defn make-installable-procedure [operator arithmetic]
  (if-let [operation (arithmetic-operation operator arithmetic)]
    (fn [& args] (apply operation args))
    (throw (ex-info "Unknown arithmetic operator" {:operator operator}))))

(defn arithmetic->bindings [arithmetic]
  (let [bindings (into {}
                       (map (fn [operator]
                              [operator (make-installable-procedure operator arithmetic)])
                            (keys (:operation-map arithmetic))))]
    (if (empty? bindings)
      (throw (ex-info "No operations found in arithmetic!" {}))
      bindings)))

(defn install-arithmetic! [arithmetic]
  (reset! current-arithmetic arithmetic)
  (install-package! (:name arithmetic) (arithmetic->bindings arithmetic)))

(defn make-operation [operator applicability procedure]
  ['operation operator applicability procedure])

(defn operation-operator [operation]
  (nth operation 1))

(defn operation-applicability [operation]
  (nth operation 2))

(defn operation-procedure [operation]
  (nth operation 3))

(defn all-args [arity predicate]
  (list (vec (repeat arity predicate))))

(defn operator-arity [f]
  (if (fn? f)
    (or (.-length f) 0)
    0))

(defn simple-operation [operator predicate procedure]
  (make-operation operator
                  (all-args (operator-arity procedure) predicate)
                  procedure))

(defn environment-lookup [env & name]
  (get-in @env name))

(defn get-implementation-value [name]
  (environment-lookup system-global-environment name))

(defn apply-operation [operation & args]
  (apply (operation-procedure operation) (first args)))

(defn eval-operation [exp]
  (let [func (first exp)
        args (rest exp)]
    (apply func args)))

(def internal-operators
  {'negate '-, 'invert '/})

(defn operator->procedure-name [operator]
  (if-let [p (internal-operators operator)]
    p
    operator))

;; ------------------------------
;; Predicate Counting and Matching (using predicate.cljs functions)
;; ------------------------------
(def predicate-counts (atom {}))
(defn reset-predicate-counts! [] (reset! predicate-counts {}))
(reset-predicate-counts!)
(defn increment-predicate-count! [predicate]
  (swap! predicate-counts update predicate (fnil inc 0)))
(defn get-predicate-count [predicate] (get @predicate-counts predicate 0))
(defn get-predicate-counts [] (seq @predicate-counts))
(defn predicates-match? [preds args]
  (try
    (and (= (count preds) (count args))
         (every? (fn [[p a]] (boolean (p a))) (map vector preds args)))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn is-applicable? [applicability args]
  (some #(predicates-match? % args) applicability))

(defn applicability-union* [applicabilities]
  (apply set/union applicabilities))
(defn applicability-union [& applicabilities]
  (applicability-union* applicabilities))
(defn is-operation-applicable? [operation args]
  (is-applicable? (operation-applicability operation) args))

(defn operation-union-dispatch [operator operations args]
  (let [operation (some #(when (is-operation-applicable? % args) %) operations)]
    (if (nil? operation)
      (println "error occur"))
    (apply-operation operation args)))

(defn operation-union* [operator operations]
  (make-operation operator
                  (applicability-union*
                   (map operation-applicability operations))
                  (fn [& args]
                    (operation-union-dispatch operator operations args))))
(defn operation-union [operator & operations]
  (operation-union* operator operations))

(defn default-object [] :default)
(defn default-object? [x] (= :default x))
(defn delete-duplicates [coll] (distinct coll))
(defn constant-union [name & constants]
  (let [unique (remove default-object? (delete-duplicates constants))]
    (if (sequential? unique)
      (first unique)
      (default-object))))
(defn make-disjunction [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))
(defn disjoin* [predicates]
  (if (empty? predicates)
    nil
    (reduce (fn [acc pred]
              (if acc (make-disjunction acc pred) pred))
            nil
            predicates)))
(defn add-arithmetics* [arithmetics]
  (if (nil? (rest arithmetics))
    (first arithmetics)
    (make-arithmetic 'add
                     (disjoin* (map :domain-predicate arithmetics))
                     arithmetics
                     constant-union
                     operation-union)))
(defn add-arithmetics [& arithmetics]
  (add-arithmetics* arithmetics))
(defn extend-arithmetic [extender base-arithmetic]
  (add-arithmetics base-arithmetic (extender base-arithmetic)))
(defn index->choices [index arity zero one]
  (loop [i 0 index index choices []]
    (if (< i arity)
      (recur (inc i) (quot index 2) (conj choices (if (odd? index) (nth one i) (nth zero i))))
      choices)))
(defn all-sequences-of [arity predicate base-predicate]
  (let [base-list (if (coll? base-predicate) base-predicate [base-predicate])
        elements (distinct (conj base-list predicate))
        pairs (for [x elements y elements] [x y])]
    (vec pairs)))
(defn any-arg [arity predicate base-predicate]
  (if (zero? arity)
    []
    (all-sequences-of arity predicate base-predicate)))

(def numeric-arithmetic
  (make-arithmetic 'numeric number? []
                   (fn [name]
                     (case name
                       additive-identity 0
                       multiplicative-identity 1
                       (default-object)))
                   (fn [operator]
                     (simple-operation operator number?
                                       (environment-lookup system-global-environment
                                                           (operator->procedure-name operator))))))
(defn symbolic-extender [base-arithmetic]
  (make-arithmetic 'symbolic symbol? [base-arithmetic]
                   (fn [name base-constant] base-constant)
                   (let [base-predicate (:domain-predicate base-arithmetic)]
                     (fn [operator base-operation]
                       (make-operation operator
                                       (any-arg (operator-arity (operation-procedure base-operation))
                                                symbol?
                                                base-predicate)
                                       (fn [& args]
                                         (conj args (operation-procedure base-operation))))))))
(defn pure-function-extender [codomain-arithmetic]
  (make-arithmetic 'pure-function fn? [codomain-arithmetic]
                   (fn [name codomain-constant] (fn [& args] codomain-constant))
                   (fn [operator codomain-operation]
                     (simple-operation operator fn?
                                       (fn [& functions]
                                         (fn [& args]
                                           (apply-operation codomain-operation
                                                            (map (fn [function] (apply function args))
                                                                 functions))))))))
(def combined-arithmetic
  (extend-arithmetic symbolic-extender numeric-arithmetic))
(install-arithmetic! (extend-arithmetic pure-function-extender combined-arithmetic))
(defn find-eval-operation [keyword & args]
  (let [operation ((environment-lookup my-env 'add keyword) 3)
        predicate ((environment-lookup my-env 'add keyword) 2)]
    (apply operation args)))

;; ------------------------------
;; Generic Procedure System
;; ------------------------------
(defrecord GenericMetadata [name arity dispatcher getter default-getter])
(defn make-generic-metadata [name arity dispatcher default-handler]
  ((dispatcher 'set-default-handler!) default-handler)
  (->GenericMetadata name arity dispatcher (dispatcher 'get-handler) (dispatcher 'get-default-handler)))
(defn generic-metadata-name [meta] (:name meta))
(defn generic-metadata-arity [meta] (:arity meta))
(defn generic-metadata-dispatch-store [meta] (:dispatcher meta))
(defn generic-metadata-getter [meta] (:getter meta))
(defn generic-metadata-default-getter [meta] (:default-getter meta))
(def generic-procedure-store (atom {}))
(def generic-procedure-extractors (atom {}))
(defn set-generic-procedure-metadata! [proc metadata]
  (swap! generic-procedure-store assoc proc metadata))
(defn %generic-procedure-metadata [proc]
  (get @generic-procedure-store proc))
(defn generic-procedure? [x]
  (contains? @generic-procedure-store x))
(defn any-object? [x] true)
(defn error-generic-procedure-handler [name]
  (fn [& args]
    (println "Inapplicable generic procedure:" name args)))
(def trace-generic-dispatch? (atom false))
(defn trace-generic-dispatch [metadata args handler]
  (when @trace-generic-dispatch?
    (println "Calling method of" (:name metadata) ":" handler (clojure.string/join " " (map pr-str args)))))
(defn generic-procedure-dispatch [metadata args]
  (let [handler ((:getter metadata) args)]
    (trace-generic-dispatch metadata args handler)
    (if handler
      (let [result (apply handler args)]
        (if (fn? result)
          (result)
          result))
      (((:default-getter metadata)) args))))
(defn generic-procedure-constructor [dispatch-store-maker]
  (fn [name arity default-handler]
    (when-not (and (integer? arity) (>= arity 0))
      (throw (ex-info "Arity must be a non-negative integer" {:arity arity})))
    (let [metadata (make-generic-metadata name arity (dispatch-store-maker)
                                          (or default-handler (error-generic-procedure-handler name)))
          the-generic-procedure (fn [& args]
                                  (generic-procedure-dispatch metadata args))]
      (set-generic-procedure-metadata! the-generic-procedure metadata)
      the-generic-procedure)))
(defn make-simple-dispatch-store []
  (let [rules (atom [])
        default-handler (atom nil)]
    (fn [message]
      (cond
        (= message 'get-handler)
        (fn [args]
          (some (fn [rule]
                  (when (and (vector? rule) (= 2 (count rule)))
                    (let [[preds handler] rule]
                      (when (predicates-match? preds args)
                        handler))))
                @rules))
        (= message 'add-handler!)
        (fn [applicability handler]
          (doseq [preds applicability]
            (let [existing (some #(and (vector? %)
                                       (= (first %) preds))
                                 @rules)]
              (if existing
                (swap! rules #(cons [preds handler] (remove (fn [x] (= (first x) preds)) %)))
                (swap! rules conj [preds handler]))))
          nil)
        (= message 'get-default-handler)
        (fn [] @default-handler)
        (= message 'set-default-handler!)
        (fn [h] (reset! default-handler h))
        (= message 'get-rules)
        (fn [] @rules)
        :else (throw (ex-info "Unknown message" {:message message}))))))
(defn make-trie [] (atom {}))
(defn get-a-value [trie path] (get-in @trie path))
(defn set-path-value! [trie path value] (swap! trie assoc-in path value))
(defn args->key [args] (mapv identity args))
(defn make-trie-dispatch-store []
  (let [delegate (make-simple-dispatch-store)
        trie (make-trie)]
    (letfn [(get-handler [args]
              (or (get @trie (args->key args))
                  ((delegate 'get-handler) args)
                  ((delegate 'get-default-handler))))
            (add-handler! [applicability handler]
              ((delegate 'add-handler!) applicability handler)
              (doseq [path applicability]
                (swap! trie assoc (args->key path) handler)))]
      (fn [message]
        (cond
          (= message 'get-handler) get-handler
          (= message 'add-handler!) add-handler!
          (= message 'get-default-handler) (delegate 'get-default-handler)
          (= message 'set-default-handler!) (delegate 'set-default-handler!)
          :else (delegate message))))))

(defn cache-wrapped-dispatch-store [dispatch-store get-key]
  (let [cache (atom {})
        base-get-handler (dispatch-store 'get-handler)]
    (let [memoized-handler
          (fn [args]
            (let [key (map get-key args)]
              (if-let [cached (@cache key)]
                cached
                (let [h (base-get-handler args)]
                  (swap! cache assoc key h)
                  h))))]
      (fn [message]
        (cond
          (= message 'get-handler) memoized-handler
          :else (dispatch-store message))))))

;; ------------------------------
;; Predicate related functions (from predicate.cljs)
;; ------------------------------
;; (We now require predicate.cljs in our namespace and use its functions as pred/..., e.g. pred/guarantee.)
;; The predicate functions have been moved to sense-reader.generic-procedure.predicate

;; ------------------------------
;; Generic Arithmetic and Generic Procedure Creation
;; ------------------------------
(defn make-generic-arithmetic [dispatch-store-maker]
  (let [make-generic-procedure (generic-procedure-constructor dispatch-store-maker)]
    (make-arithmetic 'generic
                     any-object?
                     []
                     constant-union
                     (fn [operator]
                       (simple-operation operator
                                         any-object?
                                         (make-generic-procedure operator
                                                                 (operator-arity operator)
                                                                 false))))))
(defn define-generic-procedure-extractor [name extractor]
  (swap! generic-procedure-extractors assoc name extractor))
(defn generic-procedure-metadata [object]
  (letfn [(try-object [candidate]
            (cond
              (generic-procedure? candidate) (%generic-procedure-metadata candidate)
              (seq @generic-procedure-extractors)
              (when-let [[_ extractor]
                         (some (fn [[_ f]]
                                 (when-let [val (f candidate)]
                                   [true val]))
                               @generic-procedure-extractors)]
                (try-object extractor))
              :else (throw (ex-info "Not a generic procedure" {:object candidate}))))]
    (try-object object)))
(defn generic-procedure-rules [proc]
  (let [dispatch-store ((generic-metadata-dispatch-store (generic-procedure-metadata proc)))]
    ((dispatch-store :get-rules))))
(defn generic-procedure-handlers [proc]
  (map second (generic-procedure-rules proc)))
(defn define-generic-procedure-handler [generic-procedure applicability handler]
  (let [dispatcher (:dispatcher (generic-procedure-metadata generic-procedure))
        add-handler-fn (dispatcher 'add-handler!)
        setted-arity (:arity (generic-procedure-metadata generic-procedure))]
    (if (= (count (first applicability)) setted-arity)
      (add-handler-fn applicability handler)
      (println "arity error" (:name (generic-procedure-metadata generic-procedure))
               "expected" (count (first applicability)) "input" setted-arity))))
(def simple-generic-procedure
  (generic-procedure-constructor make-simple-dispatch-store))

;; ------------------------------
;; Sample Usage: Generic Procedure 'describe'
;; ------------------------------
(set-predicate<=! integer? number?)
(set-predicate<=! even? integer?)

(def describe
  (most-specific-generic-procedure "describe" 1 (fn [x] (str "Unknown type: " x))))
(define-generic-procedure-handler describe
  [[integer?]]
  (fn [n] (str "Integer: " n)))
(define-generic-procedure-handler describe
  [[even?]]
  (fn [n] (str "Even integer: " n)))
(define-generic-procedure-handler describe
  [[number?]]
  (fn [n] (str "Generic number: " n)))
(define-generic-procedure-handler describe
  [[string?]]
  (fn [s] (str "String: \"" s "\"")))

(def describe2
  (most-specific-generic-procedure "describe" 2 (fn [x y] (str "Unknown type: " x y))))
(define-generic-procedure-handler describe2
  [[integer? integer?]]
  (fn [a b] (- a b)))
(define-generic-procedure-handler describe2
  [[even? even?]]
  (fn [a b] (+ a b)))
(define-generic-procedure-handler describe2
  [[number? number?]]
  (fn [a b] (* a b)))

;; ------------------------------
;; Guarantee functions (if not used from predicate.cljs, but could be shared)
;; ------------------------------
(defn guarantee
  ([pred value]
   (if (pred value)
     value
     (throw (ex-info (str "Value does not satisfy predicate: " value)
                     {:value value :predicate pred}))))
  ([pred value msg]
   (if (pred value)
     value
     (throw (ex-info msg {:value value :predicate pred})))))

(defn guarantee-list-of [pred coll]
  (doseq [x coll]
    (guarantee pred x (str "An element in the collection does not satisfy predicate: " pred)))
  coll)
