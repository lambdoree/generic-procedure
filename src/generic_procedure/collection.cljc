(ns generic-procedure.collection)

(defn weak-list->list [lst] lst)

(defn weak-memq [el coll]
  (some #(= el %) coll))

(defn weak-cons [el coll]
  (cons el coll))

(defn weak-car [pair]
  (first pair))

(defn weak-cdr [pair]
  (second pair))

(defn weak-pair-car? [pair]
  true)

(defn weak-set-cdr! [pair val]
  (throw (ex-info "weak-set-cdr! not implemented" {})))

(defn make-key-weak-eqv-hash-table []
  (atom {}))

(defn make-weak-eq-set []
  (let [elements (atom '())]
    (fn [op]
      (case op
        :get-elements
        (fn [] (weak-list->list @elements))

        :has-element?
        (fn [el] (boolean (weak-memq el @elements)))

        :add-element!
        (fn [el]
          (when-not (weak-memq el @elements)
            (reset! elements (weak-cons el @elements))))

        (throw (ex-info (str "Unknown operator: " op) {}))))))

(defn make-alist-store [key=?]
  (let [alist (atom '())]
    (fn [op]
      (case op
        :get-keys
        (fn [] (map first @alist))

        :has?
        (fn [k] (some #(key=? (first %) k) @alist))

        :get
        (fn [k]
          (let [p (some #(when (key=? (first %) k) %) @alist)]
            (if-not p
              (throw (ex-info (str "Unknown key: " k) {}))
              (second p))))

        :get-matching
        (fn [pred]
          (keep (fn [[k v]] (when (pred k) v)) @alist))

        :put!
        (fn [k v]
          (reset! alist
                  (cons [k v] (remove #(key=? (first %) k) @alist)))
          k)

        (throw (ex-info (str "Unknown operator: " op) {}))))))

(defn make-hash-table-store [make-table]
  (let [table (make-table)]
    (fn [op]
      (case op
        :get-keys (fn [] (keys @table))
        :has? (fn [k] (contains? @table k))
        :get (fn [k] (get @table k))
        :put! (fn [k v] (swap! table assoc k v))
        (throw (ex-info (str "Unknown operator: " op) {}))))))

(defn make-metadata-association []
  (let [store (atom {})]
    (fn [op]
      (case op
        :put! (fn [k metadata]
                (let [existing (get @store k)]
                  (when (and existing (not= existing metadata))
                    (throw (ex-info (str "Can't change metadata for: " k) {}))))
                (swap! store assoc k metadata))
        :has? #(contains? @store %)
        :get #(get @store %)
        :get-keys #(keys @store)
        (throw (ex-info (str "Unknown operator: " op) {}))))))

(defn make-weak-metadata-association []
  (make-metadata-association)) ;; 약한 참조 대신 일반 atom map 사용

(defn hash-table-get [table key]
  (get @table key))

(defn hash-table-set! [table key value]
  (swap! table assoc key value))

(defn hash-table-exists? [table key]
  (contains? @table key))

(defn hash-table-keys [table]
  (keys @table))

(defn hash-table-clear! [table]
  (reset! table {}))

(defn hash-table-intern!
  "Like get, but if key is not found, compute and store value using `thunk`"
  [table key thunk]
  (if (hash-table-exists? table key)
    (hash-table-get table key)
    (let [val (thunk)]
      (hash-table-set! table key val)
      val)))

(defn make-hash-table
  "Creates a hash table. Optional:
   - :key-eq?   custom equality function (defaults to =)
   - :hash-fn   custom hashing (optional, not used in default impl)"
  [& {:keys [key-eq? hash-fn]
      :or   {key-eq? =}}]
  (let [table (atom [])]
    (fn [op]
      (case op
        :get-keys (fn []
                    (map first @table))
        :has? (fn [k]
                (some #(key-eq? k (first %)) @table))
        :get (fn [k]
               (some (fn [[kk vv]] (when (key-eq? k kk) vv)) @table))
        :put! (fn [k v]
                (swap! table (fn [pairs]
                               (cons [k v] (remove #(key-eq? k (first %)) pairs))))
                k)
        (throw (ex-info (str "Unknown op: " op) {}))))))

