(ns generic-procedure.test-runner
  (:require [generic-procedure.generics :as generics]
            [generic-procedure.generic-procedures :as gp]
            [generic-procedure.predicates :as predicates]))

;; IO Data Structures
(defrecord IO [thunk])
(defn io? [x] (instance? IO x))

(defn init []
  ;; 1. Predicate init
  (predicates/initialize-predicate-hierarchy!)

  ;; 2. Conjoin / Disjoin Compound Operator
  (predicates/define-compound-operator-registrar!
    :conjoin
    (predicates/joinish
     (fn [tags continue]
       (or (some predicates/bottom-tag? tags)
           (continue
            (fn [joint-tag tags]
              (doseq [tag tags]
                (predicates/set-tag<=! joint-tag tag))))))))

  (predicates/define-compound-operator-registrar!
    :disjoin
    (predicates/joinish
     (fn [tags continue]
       (or (some predicates/top-tag? tags)
           (continue
            (fn [joint-tag tags]
              (doseq [tag tags]
                (predicates/set-tag<=! tag joint-tag))))))))

  ;; 3. Basic Predicates
  (def my-pos?     (predicates/make-simple-predicate "pos?" pos? :always))
  (def my-even?    (predicates/make-simple-predicate "even?" even? :always))
  (def my-neg?     (predicates/make-simple-predicate "neg?" neg? :always))
  (def my-odd?     (predicates/make-simple-predicate "odd?" odd? :always))
  (def my-number?  (predicates/make-simple-predicate "number?" number? :always))
  (def my-string?  (predicates/make-simple-predicate "string?" string? :always))

  (predicates/set-predicate<=! my-even? my-number?)
  (predicates/set-predicate<=! my-odd?  my-number?)
  (predicates/set-predicate<=! my-pos?  my-number?)
  (predicates/set-predicate<=! my-neg?  my-number?)

  ;; 5. Compound Predicates
  (def even-and-pos?
    (predicates/register-compound-predicate!
     (fn [x] (and (my-even? x) (my-pos? x)))
     :conjoin
     [my-even? my-pos?]))

  (def neg-or-even?
    (predicates/register-compound-predicate!
     (fn [x] (or (my-neg? x) (my-even? x)))
     :disjoin
     [my-neg? my-even?]))

  ;; 6. Generic Procedure with Compound Predicate
  (def analyze
    (generics/most-specific-generic-procedure "analyze" 1 (constantly "unknown")))

  (gp/define-generic-procedure-handler analyze [[even-and-pos?]]
    (fn [x] "even & positive"))

  (gp/define-generic-procedure-handler analyze [[neg-or-even?]]
    (fn [x] "either negative or even"))

  (gp/define-generic-procedure-handler analyze [[number?]]
    (fn [x] "just a number"))

  (println "== Analyze Tests ==")
  (println "analyze 4 =>" (analyze 4))
  (println "analyze -2 =>" (analyze -2))
  (println "analyze 3 =>" (analyze 3))
  (println "analyze \"hi\" =>" (analyze "hi"))

  ;; 8. Parametric Tag (ListOf, IO)
  (defn make-list-of [elem-tag]
    (predicates/make-parametric-tag
     (str "ListOf[" (predicates/tag-name elem-tag) "]")
     sequential?
     :always
     :ListOf
     [elem-tag]))

  (def list-of-even-tag   (make-list-of (predicates/predicate->tag my-even?)))
  (def list-of-string-tag (make-list-of (predicates/predicate->tag my-string?)))
  
  (def my-list-of-even?   (predicates/tag->predicate list-of-even-tag))
  (def my-list-of-string? (predicates/tag->predicate list-of-string-tag))

  (defn make-io-of [inner-tag]
    (predicates/make-parametric-tag
     (str "IO[" (predicates/tag-name inner-tag) "]")
     io?
     :always
     :IO
     [inner-tag]))

  (def io-list-of-even-tag   (make-io-of list-of-even-tag))
  (def io-list-of-string-tag (make-io-of list-of-string-tag))

  (def my-io-list-of-even?   (predicates/tag->predicate io-list-of-even-tag))
  (def my-io-list-of-string? (predicates/tag->predicate io-list-of-string-tag))

  ;; 9. Generic Procedure with Parametric Tag
  (def perform
    (generics/most-specific-generic-procedure
     "perform" 1
     (fn [x] (str "Unknown monad: " x))))

  (gp/define-generic-procedure-handler perform [[my-io-list-of-even?]]
    (fn [^IO io]
      (let [lst ((:thunk io))]
        (str "Even list from IO: " lst))))

  (gp/define-generic-procedure-handler perform [[my-io-list-of-string?]]
    (fn [^IO io]
      (let [lst ((:thunk io))]
        (str "String list from IO: " lst))))

  (def io-evens   (->IO (fn [] [2 4 6])))
  (def io-strings (->IO (fn [] ["hello" "world"])))
  (def io-unknown (->IO (fn [] :not-a-list)))

  (println "== Perform Tests ==")
  (println "perform io-evens =>" (perform io-evens))
  (println "perform io-strings =>" (perform io-strings))
  (println "perform io-unknown =>" (perform io-unknown))

  ;; 11. Chaining Generic Procedure
  (def compute
    (generics/chaining-generic-procedure "compute" 1 identity))

  (gp/define-generic-procedure-handler compute
    [[my-even?]]
    (fn [next x]
      (next (+ x 1))))

  (gp/define-generic-procedure-handler compute
    [[my-odd?]]
    (fn [next x]
      (str "Final value: " x)))

  (gp/define-generic-procedure-handler compute
    [[my-number?]]
    (fn [next x]
      (next (* 2 x))))

  ;; 12. Compute 테스트
  (println "== Compute Tests ==")
  (println (compute 4)) ; even -> next(+1) -> 5 => odd -> str "Final value: 5"
  (println (compute 5)) ; odd -> str "Final value: 5"
  )

(defn -main [& _]
  (init))
