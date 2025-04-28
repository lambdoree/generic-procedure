(ns generic-procedure.operations
  (:require [generic-procedure.applicability :as applicability]
            [generic-procedure.operator :as operator]
            [generic-procedure.util :as util]
            [generic-procedure.operations :as operations]))




(defn make-operation [operator applicability procedure]
  ['operation operator applicability procedure])

(defn operation-operator [operation]
  (nth operation 1))

(defn operation-applicability [operation]
  (nth operation 2))

(defn operation-procedure [operation]
  (nth operation 3))

(defn is-operation-applicable? [operation args]
  (applicability/is-applicable? (operation-applicability operation) args))

(defn operation-union-dispatch [operator operations args]
  (let [operation (some #(when (is-operation-applicable? % args) %) operations)]
    (if (nil? operation)
      (println "error occur"))
    (operations/apply-operation operation args)))

(defn operation-union* [operator operations]  
  (make-operation operator
                  (applicability/applicability-union*
                   (map operation-applicability operations))
                  (fn [& args]
                    (operation-union-dispatch operator
                                              operations
                                              args))))

(defn operation-union [operator & operations]
  (operation-union* operator operations))

(defn apply-operation [operation & args]
  (apply (operation-procedure operation) (first args)))

(defn simple-operation [operator predicate procedure]
  (make-operation operator
                  (applicability/all-args (operator/operator-arity procedure)
                                          predicate)
                  procedure))
(defn constant-union [name & constants]
  (let [unique (remove util/default-object?
                       (util/delete-duplicates constants))]
    (if (sequential? unique)
      (first unique)
      (util/default-object))))
