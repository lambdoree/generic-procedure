(ns generic-procedure.applicability
  (:require [clojure.set :as set]))

(defn all-sequences-of [arity predicate base-predicate]
  (let [base-list (if (coll? base-predicate) base-predicate [base-predicate])
        elements (distinct (conj base-list predicate))
        pairs (for [x elements
                    y elements]
                [x y])]
    (vec pairs)))

(defn any-arg [arity predicate base-predicate]
  (if (zero? arity)
    []
    (all-sequences-of arity predicate base-predicate)))

(defn all-args [arity predicate]
  (list (vec (repeat arity predicate))))

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
