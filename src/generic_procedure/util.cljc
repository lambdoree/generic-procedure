(ns generic-procedure.util)

(def system-global-environment
  (atom
   {}
   ))

(defn environment-lookup [env & name]
  (get-in @env name))

(defn get-implementation-value [name]
  (environment-lookup system-global-environment name))

(defn default-object []
  :default)

(defn default-object? [x]
  (= :default x))

(defn delete-duplicates [coll]
  (distinct coll))
