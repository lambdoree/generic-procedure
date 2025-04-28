(ns generic-procedure.generics
  (:require [generic-procedure.generic-procedures :as generic-procedures]
            [generic-procedure.applicability :as applicability]
            [generic-procedure.predicates :as predicates]
            ))

(defn make-subsetting-dispatch-store-maker [choose-handler]
  (fn []
    (let [delegate (generic-procedures/make-simple-dispatch-store)]
      (letfn [(get-handler [args]
                (let [rules ((delegate 'get-rules))
                      matching (filter (fn [rule]
                                         (and (vector? rule)
                                              (vector? (first rule))
                                              (fn? (second rule))
                                              (applicability/predicates-match? (first rule) args)))
                                       rules)]
                  (when (seq matching)
                    (choose-handler
                     (sort-by first #(predicates/compare-applicability %2 %1) matching)
                     ((delegate 'get-default-handler))))))]
        (fn [message]
          (cond
            (= message 'get-handler) get-handler
            :else (delegate message)))))))

(defn make-chaining-dispatch-store []
  ((make-subsetting-dispatch-store-maker
    (fn [handlers default-handler]
      (letfn [(loop-handlers [handlers]
                (if (seq handlers)
                  (let [[_ handler] (first handlers)
                        next-handler (loop-handlers (rest handlers))]
                    (fn [& args]
                      (apply handler next-handler args)))
                  default-handler))]
        (loop-handlers handlers))))))


(defn make-most-specific-dispatch-store []
  ((make-subsetting-dispatch-store-maker (fn [handlers _] (second (first handlers))))))

(defn make-cached-most-specific-dispatch-store []
  (generic-procedures/cache-wrapped-dispatch-store (make-most-specific-dispatch-store) identity))

(def most-specific-generic-procedure
  (generic-procedures/generic-procedure-constructor make-cached-most-specific-dispatch-store))

(defn make-cached-chaining-dispatch-store []
  (generic-procedures/cache-wrapped-dispatch-store (make-chaining-dispatch-store) identity))

(def chaining-generic-procedure
  (generic-procedures/generic-procedure-constructor make-cached-chaining-dispatch-store))



