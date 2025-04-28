(ns generic-procedure.operator)

(def internal-operators
  {'negate '-, 'invert '/})

(defn operator->procedure-name [operator]
  (if-let [p (internal-operators operator)]
    p
    operator))
(defn operator-arity [f]
  (if (fn? f)
    (or (.-length f) 0)
    0))
