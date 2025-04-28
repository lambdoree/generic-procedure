(ns generic-procedure.tagging
  (:require [clojure.set :as set]))

(defonce predicate-tags (atom {}))
(defonce tag-relations (atom {}))

(defrecord TaggedData [tag data])

(defn %make-tagged-data [tag data]
  (->TaggedData tag data))

(defn tagged-data? [object]
  (instance? TaggedData object))

(defn tagged-data-tag [td]
  (:tag td))

(defn tagged-data-data [td]
  (:data td))

(defn tag-constructor [tag]
  (fn [data]
    (%make-tagged-data tag data)))

(defn get-tag [object]
  (if (tagged-data? object)
    (:tag object)
    nil))

(defn all-super-tags [tag]
  (loop [visited #{tag}
         to-visit (get @tag-relations tag #{})]
    (if (empty? to-visit)
      (disj visited tag)
      (let [current (first to-visit)
            neighbors (get @tag-relations current #{})
            new-to-visit (set/difference neighbors visited)]
        (recur (conj visited current)
               (set/union (disj to-visit current) new-to-visit))))))

(defn tag<= [tag1 tag2]
  (or (= tag1 tag2)
      (contains? (all-super-tags tag1) tag2)))

;; tagging-strategy:never
(defn tagging-strategy-never [name data-test make-tag]
  (let [constructor (fn [data]
                      (if (not (data-test data))
                        (throw (ex-info (str "Ill-formed data for " name ": " data) {}))
                        data))
        tag (make-tag data-test constructor (fn [object] object))]
    (swap! predicate-tags assoc data-test tag)
    tag))

;; tagging-strategy:always
(defn tagging-strategy-always [name data-test make-tag]
  (let [tag (atom nil)
        constructor (fn [data]
                      (if (not (data-test data))
                        (throw (ex-info (str "Ill-formed data for " name ": " data) {}))
                        (%make-tagged-data @tag data)))
        internal-predicate (fn [object]
                             (and (tagged-data? object)
                                  (tag<= (tagged-data-tag object) @tag)
                                  (data-test (tagged-data-data object))))
        accessor (fn [object]
                   (if (tagged-data? object)
                     (tagged-data-data object)
                     object))
        pred-fn (fn [x]
                  (if (tagged-data? x)
                    (internal-predicate x)
                    (data-test x)))
        computed-tag (make-tag pred-fn constructor accessor)]
    (reset! tag computed-tag)
    (swap! predicate-tags assoc pred-fn computed-tag)
    computed-tag))

;; tagging-strategy:optional
(defn tagging-strategy-optional [name data-test make-tag]
  (let [tag (atom nil)
        constructor (fn [data]
                      (if (not (data-test data))
                        (throw (ex-info (str "Ill-formed data for " name ": " data) {}))
                        (if (= @tag (get-tag data))
                          data
                          (%make-tagged-data @tag data))))
        predicate (fn [object]
                    (or (and (tagged-data? object)
                             (tag<= (tagged-data-tag object) @tag)
                             (data-test (tagged-data-data object)))
                        (data-test object)))
        accessor (fn [object]
                   (if (tagged-data? object)
                     (tagged-data-data object)
                     object))
        pred-fn (fn [x]
                  (if (tagged-data? x)
                    (predicate x)
                    (data-test x)))
        computed-tag (make-tag pred-fn constructor accessor)]
    (reset! tag computed-tag)
    (swap! predicate-tags assoc pred-fn computed-tag)
    computed-tag))



