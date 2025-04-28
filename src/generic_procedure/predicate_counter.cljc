(ns generic-procedure.predicate-counter)

(def predicate-counts (atom {}))

(defn reset-predicate-counts! []
  (reset! predicate-counts {}))

(reset-predicate-counts!)

(defn increment-predicate-count! [predicate]
  (swap! predicate-counts update predicate (fnil inc 0)))

(defn get-predicate-count [predicate]
  (get @predicate-counts predicate 0))

(defn get-predicate-counts []
  (seq @predicate-counts))
