; Project Euler Problem 5
;
;   2520 is the smallest number that can be divided by each of the numbers 
;   from 1 to 10 without any remainder.  What is the smallest positive 
;   number that is evenly divisible by all of the numbers from 1 to 20?
;
;   Answer: 232792560

;;##################################################
;;--------------------------------------------------
;; Brute force and super slow
;;--------------------------------------------------

(defn pe5 [start rng]
  (loop [x start]
    (if (every? #(zero? (rem x %)) rng)
      x
      (recur (inc x)))))
;;##################################################


;;##################################################
;;--------------------------------------------------
;; Using prime factors.  Super fast
;;--------------------------------------------------

;;--------------------------------------------------
;; (logarithm {:base 2 :number 8})
;;--------------------------------------------------
(defn logarithm [{x :number b :base}] 
  "Return the log base b of x"
  (/ (Math/log x) (Math/log b)))

;;--------------------------------------------------
;; TODO: implement this instead of hard code it
;;--------------------------------------------------
(defn primes [n]
  "Return a coll of all primes < n"
  [2 3 5 7 11 13 17 19])

(defn pe5-pf [k]
  (reduce * (map #(Math/pow % (Math/floor (logarithm {:base % :number k}))) 
                 (primes k))))
;;##################################################


(defn enqueue  [sieve n factor]
  (let [m (+ n factor)]
    (if (sieve m)
      (recur sieve m factor)
      (assoc sieve m factor))))
  
(defn next-sieve [sieve candidate]
  (if-let [factor (sieve candidate)]
    (-> sieve
      (dissoc candidate)
      (enqueue candidate factor))
    (enqueue sieve candidate candidate)))

  
(defn primes [max]
    (apply concat (vals (reduce next-sieve {} (range 2 max)))))

