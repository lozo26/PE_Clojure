; Project Euler Problem 6
;
;   The sum of the squares of the first ten natural numbers is, 12 + 22 + ... + 102 = 385
;   The square of the sum of the first ten natural numbers is, (1 + 2 + ... + 10)2 = 552 = 3025
;   Hence the difference between the sum of the squares of the first ten natural numbers and 
;   the square of the sum is 3025 − 385 = 2640.
;
;   Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
;
;
;   Answer: 25164150

(a + b)(a + b)
a^2 + 2ab + b^2

(a + b + c)(a + b + c)
a^2 + ab + ac + ba + b^2 + bc + ca + cb + c^2
a^2 + b&2 + c^2 + 2ab + 2bc + 2ac

(a + b + c + d)(a + b + c + d)
a^2 + ab + ac + ad + ba + b^2 + bc + bd + ca + cb + c^2 + cd + da + db + dc + d^2
a^2 + b^2 + c^2 + d^2 + 2ab + 2ca + 2ad + 2bc + 2bd + 2cd

;;##################################################
;;--------------------------------------------------
;; Use FOIL to find pattern 2*sum(each combo).  21.782 msecs
;;--------------------------------------------------

(defn combos [lst]
  (let [sub-combos (fn sub-combos [lst] (map #(list (first lst) %) (rest lst)))]
    (loop [l lst, c []]
      (if (empty? l)
        c
        (recur (rest l) (concat c (sub-combos l)))))))
      
(reduce + (map #(* 2 (first %) (second %)) (combos (range 1 101))))


;;##################################################
;;--------------------------------------------------
;; Brute force. .309 msecs
;;--------------------------------------------------

(defn sum-of-squares [lst]
  (reduce + (map #(Math/pow % 2) lst)))

(defn square-of-sums [lst]
  (Math/pow (reduce + lst) 2))

(defn diff [lst]
  (- (square-of-sums lst) (sum-of-squares lst)))

;;##################################################
;;--------------------------------------------------
;; Formula .039 msecs
;;--------------------------------------------------
(defn formula [limit]
  (let [sum (/ (* limit (inc limit)) 2)
        sum-sq (/ (* limit (inc (* limit 2)) (inc limit)) 6)]
    (- (* sum sum) sum-sq)))
    
