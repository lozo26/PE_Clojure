; Project Euler Problem 1
;    The prime factors of 13195 are 5, 7, 13 and 29.
;    What is the largest prime factor of the number 600851475143 ?
;
;    Answer: 6857

;; Algorithm:
;;  Recursively find factors of factors starting from smallest.
;;  The last factor we find will be the largest prime factor.

(defn step [n]
  (if (= n 2)
    3
    (+ n 2)))

(defn largest-factor [num]
  ; Special case for d=2
  (loop [n num]
    (if (> 4 n) ;4=2*2
      2
      (if (even? n) (recur (/ n 2)))))

  ; Use step=2 for d >= 3
  (loop [n num, d 3]
    (if (> (* d d) n)
      n
      (if (zero? (rem n d))
        (recur (/ n d) d)
        (recur n (+ 2 d))))))

(defn pe3 [] (largest-factor 600851475143))

(time (dotimes [_ 5000] (pe3)))
