;; Produces a sequence of sequences such that each sequence is the previous
;; sequence minus the first element.  (i.e. sequence of "tails")
;;    =>(tails (range 5))
;;    ((0 1 2 3 4) (1 2 3 4) (2 3 4) (3 4) (4))
(defn tails [x]               
  (take (count x) 
        (iterate next x)))

(defn maptails [f x]
  (take (count x)
        (iterate #(f (next %)) x)))

(defn sub-combos [x] 
  (map #(list (first x) %) 
       (rest x)))

;; The following two are equivalent:
;;  Works well if map results in a sequence of sequences that you would like flattened
;;  one level.
(mapcat :some-func :some-seq)
(apply concat (map :some-func :some-seq))

;; Debug macro
(defmacro dbg [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

