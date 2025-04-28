(ns generic-procedure.trie)

(defn make-trie [] (atom {}))
(defn get-a-value [trie path] (get-in @trie path))
(defn set-path-value! [trie path value] (swap! trie assoc-in path value))
