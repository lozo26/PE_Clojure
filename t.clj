;; Round 1 => (2, 3)
(next-primes {} 3)
  -> (next-primes (next-sieve {} 3) 5)
    -> (next-sieve {} 3)
      -> (enqueue {} 3 6)
  -> (next-primes {9 6} 5)

;; Round 2 => (2, 3, 5)
(next-primes {9 6} 5)
  -> (next-primes (next-sieve {9 6} 5) 7)
    -> (next-sieve {9 6 } 5)
      -> (enqueue {9 6} 5 10)
  -> (next-primes {15 10, 9 6} 7)

;; Round 3 => (2, 3, 5, 7)
(next-primes {15 10, 9 6} 7)
  -> (next-primes (next-sieve {15 10, 9 6} 7) 9)
    -> (next-sieve {15 10, 9 6} 7)
      -> (enqueue {15 10, 9 6} 7 14)
  -> (next-primes {21 14, 9 6, 15 10} 9)

;; Round 4 => (2, 3, 5, 7, 11)
(next-primes {21 14, 9 6, 15 10} 9)
  -> (recur (next-sieve {21 14, 9 6, 15 10} 9) 11)
    -> (next-sieve {21 14, 9 6, 15 10} 9)
      -> (enqueue {21 14, 15 10} 9 6)
  -> (recur {21 14, 15 10, 27 6} 11)
  -> (next-primes {21 14, 15 10, 27 6} 13)

m = 9 + 6 = 15
(recur sieve 15 6)
sieve = {21 14, 15 10}
m = 15 + 6 = 21
(recur sieve 21 6)
m = 21 + 6 = 27
{21 14, 15 10, 27 6}



(defn enqueue [sieve n factor]
  (let [m (+ n factor)]
    (assoc sieve m
           (conj (sieve m) factor))))

(defn next-sieve [sieve candidate]
  (if-let [factors (sieve candidate)]
    (reduce #(enqueue %1 candidate %2)
            (dissoc sieve candidate)
            factors)
    (enqueue sieve candidate candidate)))
