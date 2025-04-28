(ns generic-procedure.predicates
  (:require [generic-procedure.collection :as collection]
            [generic-procedure.tagging :as tagging]
            [clojure.set :as set]
            ))

(defrecord TagShared [name predicate constructor accessor supersets])
(defrecord SimpleTag [shared])
(defrecord CompoundTag [shared operator components])
(defrecord ParametricTag [shared template bindings])

(defn tag-shared? [x] (instance? TagShared x))
(defn simple-tag? [x] (instance? SimpleTag x))
(defn compound-tag? [x] (instance? CompoundTag x))
(defn parametric-tag? [x] (instance? ParametricTag x))

(defn get-tag-shared [tag] (:shared tag))

(defn tag-shared-name [x] (:name x))
(defn tag-shared-predicate [x] (:predicate x))
(defn tag-shared-constructor [x] (:constructor x))
(defn tag-shared-accessor [x] (:accessor x))
(defn tag-shared-supersets [x] (:supersets x))

(defn simple-tag-shared [x] (:shared x))
(defn compound-tag-shared [x] (:shared x))
(defn compound-tag-operator [x] (:operator x))
(defn compound-tag-components [x] (:components x))
(defn parametric-tag-shared [x] (:shared x))
(defn parametric-tag-template [x] (:template x))
(defn parametric-tag-bindings [x] (:bindings x))

(defn any-object? [_] true)
(defn no-object? [_] false)

(defn guarantee
  ([pred val]
   (if (pred val) val
       (throw (ex-info "Value does not satisfy predicate" {:value val :predicate pred}))))
  ([pred val msg]
   (if (pred val) val
       (throw (ex-info msg {:value val :predicate pred})))))

(defn guarantee-list-of [pred coll]
  (doseq [x coll]
    (guarantee pred x (str "Element does not satisfy predicate: " x)))
  coll)

(defn tag-name [tag] (tag-shared-name (get-tag-shared tag)))
(defn tag-constructor [tag] (tag-shared-constructor (get-tag-shared tag)))
(defn tag-accessor [tag] (tag-shared-accessor (get-tag-shared tag)))
(defn tag->predicate [tag] (tag-shared-predicate (get-tag-shared tag)))
(defn tag-supersets [tag] (tag-shared-supersets (get-tag-shared tag)))

(defn predicate->tag [pred]
  (or (get @tagging/predicate-tags pred)
      (let [tag (gensym "tag")]
        (swap! tagging/predicate-tags assoc pred tag)
        tag)))

(defn predicate-constructor [pred]
  (tag-constructor (predicate->tag pred)))

(defn predicate-accessor [pred]
  (tag-accessor (predicate->tag pred)))

(defn tag<=? [t1 t2]
  (or (= t1 t2)
      (contains? (tagging/all-super-tags t1) t2)))

(defn tag>=? [t1 t2]
  (tag<=? t2 t1))

(defn tag=? [t1 t2]
  (= t1 t2))

(defn set-tag<=! [t1 t2]
  (if (tag>=? t1 t2)
    (println (ex-info "Cannot create cycle in tag hierarchy" {:from t1 :to t2})))
  (when-not (tag<=? t1 t2)
    (swap! tagging/tag-relations update t1 (fnil conj #{}) t2)))

(defn predicate<=? [p1 p2]
  (tag<=? (predicate->tag p1) (predicate->tag p2)))

(defn set-predicate<=! [p1 p2]
  (set-tag<=! (predicate->tag p1) (predicate->tag p2)))

(defn set-predicate-tag! [pred tag]
  (swap! tagging/predicate-tags assoc pred tag))

(defn predicate=? [p1 p2]
  (= (predicate->tag p1)
     (predicate->tag p2)))

(defn predicate-supersets [pred]
  (map tag->predicate
       (tag-supersets (predicate->tag pred))))

(defn all-predicate-supersets [pred]
  (letfn [(walk [queue seen]
            (if (empty? queue)
              seen
              (let [tag (first queue)
                    rest (rest queue)
                    supers (tagging/all-super-tags tag)
                    new-tags (remove seen supers)]
                (recur (into rest new-tags)
                       (into seen new-tags)))))]
    (map tag->predicate (walk [(predicate->tag pred)] #{}))))

(defn tags->predicates [tags]
  (map tag->predicate tags))

(defn get-tag-supersets [tag]
  @(tag-supersets tag))

(defn is-more-specific? [p1 p2]
  (predicate<=? p1 p2))

(defn compare-applicability [a1 a2]
  (loop [p1 a1 p2 a2]
    (cond
      (empty? p1) (if (empty? p2) 0 -1)
      (empty? p2) 1
      :else (let [r (compare (is-more-specific? (first p1) (first p2))
                             (is-more-specific? (first p2) (first p1)))]
              (if (zero? r)
                (recur (rest p1) (rest p2))
                r)))))

(defn invoke-tagging-strategy [strategy name pred make-tag]
  (cond
    (= strategy :always)   (tagging/tagging-strategy-always name pred make-tag)
    (= strategy :optional) (tagging/tagging-strategy-optional name pred make-tag)
    (= strategy :never)    (tagging/tagging-strategy-never name pred make-tag)
    (fn? strategy)         (strategy name pred make-tag)
    :else (throw (ex-info "Unknown tagging strategy" {:strategy strategy}))))

(defn make-tag-shared [name pred ctor accessor]
  (->TagShared name pred ctor accessor (atom #{})))

(defn make-simple-tag [name data-test strategy]
  (invoke-tagging-strategy strategy name data-test
                           (fn [pred ctor accessor]
                             (->SimpleTag (make-tag-shared name pred ctor accessor)))))

(defn make-simple-predicate [name data-test strategy]
  (tag->predicate
   (make-simple-tag name data-test strategy)))

(defn simple-abstract-predicate [name data-test]
  (make-simple-predicate name data-test :always))

(defn make-compound-tag [name data-test strategy operator components]
  (invoke-tagging-strategy strategy name data-test
                           (fn [pred ctor accessor]
                             (->CompoundTag
                              (make-tag-shared name pred ctor accessor)
                              operator
                              components))))

(defonce compound-operator-registrars (atom {}))

(defn define-compound-operator-registrar! [operator f]
  (swap! compound-operator-registrars assoc operator f))

(defn get-compound-operator-registrar [operator]
  (get @compound-operator-registrars operator))

(defn register-predicate! [predicate name]
  (guarantee fn? predicate)
  (make-simple-predicate name predicate :never))

(defn register-compound-predicate! [joint-predicate operator components]
  (guarantee fn? joint-predicate)
  (guarantee vector? components) ;; cljs에서는 list보다 vector를 기본으로 씀
  (let [registrar (get-compound-operator-registrar operator)]
    (tag->predicate
     (registrar joint-predicate operator (mapv predicate->tag components)))))

(defn compound-predicate? [object]
  (and (fn? object)
       (compound-tag? (predicate->tag object))))

(defn compound-predicate-components [predicate]
  (map tag->predicate
       (compound-tag-components (predicate->tag predicate))))

(defn compound-predicate-predicate [operator]
  (fn [object]
    (and (fn? object)
         (let [tag (predicate->tag object)]
           (and (compound-tag? tag)
                (= (compound-tag-operator tag) operator))))))

(def disjunction? (compound-predicate-predicate :disjoin))
(def conjunction? (compound-predicate-predicate :conjoin))

(defn joinish [wrap-constructor]
  (fn [data-test operator tags]
    (let [tags (->> tags
                    (mapcat (fn [tag]
                              (if (and (compound-tag? tag)
                                       (= (compound-tag-operator tag) operator))
                                (compound-tag-components tag)
                                [tag])))
                    (distinct))]
      (if (= 1 (count tags))
        (first tags)
        (wrap-constructor tags
                          (fn [post-process]
                            (let [tag (make-compound-tag
                                       (cons operator (map tag-name tags))
                                       data-test
                                       :optional
                                       operator
                                       tags)]
                              (post-process tag tags)
                              tag)))))))

(defn make-parametric-tag [name data-test strategy template bindings]
  (invoke-tagging-strategy strategy name data-test
                           (fn [pred ctor accessor]
                             (->ParametricTag
                              (make-tag-shared name pred ctor accessor)
                              template
                              bindings))))

(defn parametric-predicate? [obj]
  (and (fn? obj)
       (parametric-tag? (predicate->tag obj))))

(defn parametric-predicate-template [pred]
  (parametric-tag-template (predicate->tag pred)))

(defonce top-tag (atom nil))
(defonce bottom-tag (atom nil))

(defn initialize-predicate-hierarchy! []
  (let [any (simple-abstract-predicate 'any-object? any-object?)
        none (simple-abstract-predicate 'no-object? no-object?)]
    (reset! top-tag (predicate->tag any))
    (reset! bottom-tag (predicate->tag none))))

(defn top-tag? [tag]
  (= tag @top-tag))

(defn bottom-tag? [tag]
  (= tag @bottom-tag))
