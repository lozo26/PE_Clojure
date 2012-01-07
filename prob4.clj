;--------------------------------------------------
;--------------------------------------------------
(defn products [st end]
    (apply concat 
           (map (fn [x] (map #(* x %) (range x end)))
                (range st end))))

(defn palindrome? [n]
  (= (seq (str n)) (reverse (str n))))

(defn pe4 []
  (apply max (filter palindrome? (products 100 999))))

;--------------------------------------------------
;--------------------------------------------------
(defn palindromes [st end]
    (for [a (range st end)
          b (range a end)
          :when (palindrome? (* a b))]
      (* a b)))

(defn pe4-for [] (apply max (palindromes 100 999)))

