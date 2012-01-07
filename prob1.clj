; Project Euler Problem 1
;
;    If we list all the natural numbers below 10 that are multiples of 3 or 5, 
;    we get 3, 5, 6 and 9. The sum of these multiples is 23.
;    Find the sum of all the multiples of 3 or 5 below 1000.
;
;   Answer: 233168

;;--------------------------------------------------
;; Helper predicate function to test if an integer
;; is a multiple of 3 or 5.
;;  -- Helps keep code more readable by not having
;;     to embed this logic in every implementation.
;;--------------------------------------------------
(defn mul3-or-mul5? [n]
  (or (zero? (rem n 3)) 
      (zero? (rem n 5))))
;;--------------------------------------------------

;;--------------------------------------------------
;; Tail recursive version 
;;  -- Very verbose and not very readable. Probably
;;     the worst option of these three.
;;--------------------------------------------------
(defn e1-tc [n]
  (loop [n (dec n)
         acc 0]
    (if (= n 0)
      acc
      (recur (dec n) 
             (if (mul3-or-mul5? n) 
               (+ acc n)
               acc)))))
;;--------------------------------------------------

;;--------------------------------------------------
;; Filter, reduce version 
;;  -- Best version in my opinion. Most readable.
;;--------------------------------------------------
(defn e1-filt [n]
  (reduce + (filter mul3-or-mul5? (range n))))
;;--------------------------------------------------

;;--------------------------------------------------
;; Seq comprehension version -- 2nd best
;;  -- A little longer than the filter version and 
;;     not as readable.
;;--------------------------------------------------
(defn e1-comp [n]
  (reduce + 
          (for [i (range n) :when (mul3-or-mul5? i)] 
            i)))
;;--------------------------------------------------
