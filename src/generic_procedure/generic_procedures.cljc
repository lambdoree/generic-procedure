(ns generic-procedure.generic-procedures
  (:require [generic-procedure.applicability :as applicability]
            [generic-procedure.trie :as trie]
            ))

(def generic-procedure-store (atom {}))
(def generic-procedure-extractors (atom {}))
(def trace-generic-dispatch? (atom false))

(defn set-generic-procedure-metadata! [proc metadata]
  (swap! generic-procedure-store assoc proc metadata))

(defn %generic-procedure-metadata [proc]
  (get @generic-procedure-store proc))

(defn generic-procedure? [x]
  (contains? @generic-procedure-store x))

(defn error-generic-procedure-handler [name]
  (fn [& args]
    (println "Inapplicable generic procedure:" name args)))

(defrecord GenericMetadata [name arity dispatcher getter default-getter])

(defn make-generic-metadata [name arity dispatcher default-handler]
  ((dispatcher 'set-default-handler!) default-handler)
  (->GenericMetadata
   name
   arity
   dispatcher
   (dispatcher 'get-handler)
   (dispatcher 'get-default-handler)))

(defn generic-metadata-name [meta] (:name meta))
(defn generic-metadata-arity [meta] (:arity meta))
(defn generic-metadata-dispatch-store [meta] (:dispatcher meta))
(defn generic-metadata-getter [meta] (:getter meta))
(defn generic-metadata-default-getter [meta] (:default-getter meta))


(defn trace-generic-dispatch [metadata args handler]
  (when @trace-generic-dispatch?
    (println "Calling method of" (:name metadata) ":"
             handler
             (clojure.string/join " " (map pr-str args)))))

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
    (let [metadata (make-generic-metadata
                    name arity
                    (dispatch-store-maker)
                    (or default-handler
                        (error-generic-procedure-handler name)))
          the-generic-procedure (fn [& args]
                                  (generic-procedure-dispatch metadata args))]
      (set-generic-procedure-metadata! the-generic-procedure metadata)
      the-generic-procedure)))



(defonce %constant-generic-procedure-handlers (atom {}))

(defn constant-generic-procedure-handler [constant]
  (if (contains? @%constant-generic-procedure-handlers constant)
    (get @%constant-generic-procedure-handlers constant)
    (let [handler (fn [& _args] constant)]
      (swap! %constant-generic-procedure-handlers assoc constant handler)
      handler)))

(defn define-generic-procedure-extractor [name extractor]
  (swap! generic-procedure-extractors assoc name extractor))

(defn generic-procedure-metadata [object]
  (letfn [(try-object [candidate]
            (cond
              (generic-procedure? candidate) (%generic-procedure-metadata candidate)
              (seq @generic-procedure-extractors)
              (when-let [[_ extractor] (some (fn [[_ f]] (when-let [val (f candidate)] [true val])) @generic-procedure-extractors)]
                (try-object extractor))
              :else (throw (ex-info "Not a generic procedure" {:object object}))))]
    (try-object object)))

(defn generic-procedure-rules [proc]
  (let [dispatch-store ((generic-metadata-dispatch-store (generic-procedure-metadata proc)))]
    ((dispatch-store :get-rules))))

(defn generic-procedure-handlers [proc]
  (map second (generic-procedure-rules proc)))

(defn define-generic-procedure-handler [generic-procedure applicability handler]
  (let [dispatcher (:dispatcher (generic-procedure-metadata generic-procedure))
        add-handler-fn (dispatcher 'add-handler!)
        setted-arity (:arity (generic-procedure-metadata generic-procedure))
        ]
    (if (= (count (first applicability)) setted-arity)
      (add-handler-fn applicability handler)
      (println "arity error" (:name (generic-procedure-metadata generic-procedure)) "expected" (count (first applicability)) "input" setted-arity)
      )))

(defn make-simple-dispatch-store []
  (let [rules (atom [])
        default-handler (atom nil)]
    (fn [message]
      (cond
        (= message 'get-handler)
        (fn [args]
          (some (fn [rule]
                  (when (and (vector? rule)
                             (= 2 (count rule)))
                    (let [[preds handler] rule]
                      (when (applicability/predicates-match? preds args)
                        handler))))
                @rules))

        (= message 'add-handler!)
        (fn [applicability handler]
          (doseq [preds applicability]
            (let [existing (some #(and (vector? %) (= (first %) preds)) @rules)]
              (if existing
                (swap! rules #(cons [preds handler] (remove (fn [x] (= (first x) preds)) %)))
                (swap! rules conj [preds handler])))))

        (= message 'get-default-handler)
        (fn [] @default-handler)

        (= message 'set-default-handler!)
        (fn [h] (reset! default-handler h))

        (= message 'get-rules)
        (fn [] @rules)

        :else
        (throw (ex-info "Unknown message" {:message message}))))))

(defn args->key [args]
  (mapv identity args))

(def make-default-dispatch-store make-simple-dispatch-store)

(defn make-trie-dispatch-store []
  (let [delegate (make-simple-dispatch-store)
        trie (trie/make-trie)]
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
(def simple-generic-procedure
  (generic-procedure-constructor make-simple-dispatch-store))
