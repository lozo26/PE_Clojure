;;-----------------------------------------------------------------
;; primes - Naively find all primes less than max
;;    Overview:
;;      Iteratively build a map using values in range [2 - max) where 
;;      each key is a composite number and the corresponding value 
;;      is a sequence which is a subset of that composite number's 
;;      prime factors.
;;
;;      The concatenated list of prime factors--map values--is the 
;;      list of primes less than max.
;;
;;    next-sieve:
;;      Given a sieve and a candidate prime:
;;        1) First check to see that the candidate is not already
;;           a key in the sieve map which would indicate it is
;;           composite.
;;        2) If candidate is found in the sieve map:
;;            A) Remove the candidate from the sieve
;;            B) For each prime factor of candidate f:
;;                i) enqueue the sieve map with args candidate and f
;;        3) If candidate is NOT found in sieve map:
;;            enqueue the sieve map with args candidate candidate
;;
;;    enqueue:
;;      Given a sieve map, some number n which is a multiple of f,
;;      and a prime factor f:
;;        1) Let m = n+f which makes m a multiple of f
;;        2) Add or append the factor f to the map entry for m
;;
;;
;;    Example sieve progression with range 2 6:
;;      (next-sieve {} 2) 
;;          => {4 (2)}
;;      (next-sieve {4 (2)} 3)
;;          => {4 (2), 6 (3)}
;;      (next-sieve {4 (2), 6 (3)} 4)
;;          => {6 (3, 2)}
;;      (next-sieve {6 (3, 2)} 5)
;;          => {6 (3, 2), 10 (5)}
;;      (next-sieve {6 (3, 2), 10 (5)} 6)
;;          => {10 (5), 9 (3), 8 (2)}
;;
;;      Note that if we take all the values of the map, a list if lists,
;;      the "squash" it into a single level list, we'll have a list of
;;      all the primes less than 6.
;;        
;;-----------------------------------------------------------------
(defn primes [max]
  (let [enqueue (fn [sieve n factor]
                  (let [m (+ n factor)]
                    (assoc sieve m
                      (conj (sieve m) factor))))
        next-sieve (fn [sieve candidate]
                     (if-let [factors (sieve candidate)]
                       (reduce #(enqueue %1 candidate %2)
                         (dissoc sieve candidate)
                         factors)
                       (enqueue sieve candidate candidate)))]
    (apply concat (vals (reduce next-sieve {} (range 2 max))))))

;;-----------------------------------------------------------------
;; primes3 - find all primes less than max
;;    Overview:
;;      Iteratively build a map using values in range [2 - max) where 
;;      each key is a composite number and the corresponding value 
;;      is a single prime factor for that number.
;;
;;      The concatenated list of prime factors--map values--is the 
;;      list of primes less than max.
;;
;;    next-sieve:
;;      Given a sieve and a candidate prime:
;;        1) First check to see that the candidate is not already
;;           a key in the sieve map which would indicate it is
;;           composite.
;;        2) If candidate is found in the sieve map:
;;            A) Remove the candidate from the sieve
;;            B) enqueue the sieve map with a new multiple of candidate's
;;               sieve map value.
;;        3) If candidate is NOT found in sieve map:
;;            A) enqueue the sieve map with with a new multiple of candidate.
;;
;;    enqueue:
;;      Given a sieve map, some number n, and a step value such that n and step
;;      are multiples of some prime factor f:
;;        1) Let m = n+step which makes m a multiple of f
;;        2) If sieve map contains m:
;;            A) recursively enqueue n=m and step=step until we find a multiple
;;               of step not in map.
;;        3) If sieve does NOT contain m:
;;            A) Add m -> step to sieve map
;;
;;    Example sieve progression with range 2 6:
;;      (next-sieve {} 2) 
;;          => {4 2}
;;      (next-sieve {4 2} 3)
;;          => {4 2, 6 3}
;;      (next-sieve {4 2, 6 3} 4)
;;          => {6 3, 8 2)}
;;      (next-sieve {6 3, 8 2} 5)
;;          => {6 3, 8 2, 10 5}
;;      (next-sieve {6 3, 8 2, 10 5} 6)
;;          => {10 5, 9 3, 8 2}
;;
;;      Note that if we take all the values of the map, a list if lists,
;;      the "squash" it into a single level list, we'll have a list of
;;      all the primes less than 6.
;;-----------------------------------------------------------------
(defn primes3 [max]
  (let [enqueue (fn [sieve n factor]
                  (let [m (+ n (+ factor factor))]
                    (if (sieve m)
                      (recur sieve m factor)
                      (assoc sieve m factor))))
        next-sieve (fn [sieve candidate]
                     (if-let [factor (sieve candidate)]
                       (-> sieve
                         (dissoc candidate)
                         (enqueue candidate factor))
                       (enqueue sieve candidate candidate)))]
    (cons 2 (vals (reduce next-sieve {} (range 3 max 2))))))

;;-----------------------------------------------------------------
;;    lazy-primes3 - infinite lazy sequence of primes
;;-----------------------------------------------------------------
(defn lazy-primes3 []
  (letfn [(enqueue [sieve n step]
                   (let [m (+ n step)]
                     (if (sieve m)
                       (recur sieve m step)
                       (assoc sieve m step))))
          (next-sieve [sieve candidate]
                      (if-let [step (sieve candidate)]
                        (-> sieve
                          (dissoc candidate)
                          (enqueue candidate step))
                        (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
                       (if (sieve candidate)
                         (recur (next-sieve sieve candidate) 
                                (+ candidate 2))
                         (cons candidate 
                               (lazy-seq (next-primes (next-sieve sieve candidate) 
                                                      (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

;;--------------------------------------------------------------------
;; Primes2 version of sub functions
;;--------------------------------------------------------------------
(defn enqueue [sieve n factor]
  (let [m (+ n factor)]
    (if (sieve m)
      (recur sieve m factor)
      (assoc sieve m factor))))

(defn next-sieve  [sieve candidate]
  (if-let [factor (sieve candidate)]
    (-> sieve
      (dissoc candidate)
      (enqueue candidate factor))
    (enqueue sieve candidate candidate)))

;;--------------------------------------------------------------------
;; primes3 version of sub functions
;;--------------------------------------------------------------------
(defn enqueue [sieve n factor]
  (let [m (+ n (+ factor factor))]
    (if (sieve m)
      (recur sieve m factor)
      (assoc sieve m factor))))

(defn next-sieve [sieve candidate]
  (if-let [factor (sieve candidate)]
    (-> sieve
      (dissoc candidate)
      (enqueue candidate factor))
    (enqueue sieve candidate candidate)))

;;--------------------------------------------------------------------
;; lazy-primes3 version of sub functions
;;--------------------------------------------------------------------
(defn enqueue [sieve n step]
  (let [m (+ n step)]
    (if (sieve m)
      (recur sieve m step)
      (assoc sieve m step))))

(defn next-sieve [sieve candidate]
  (if-let [step (sieve candidate)]
    (-> sieve
      (dissoc candidate)
      (enqueue candidate step))
    (enqueue sieve candidate (+ candidate candidate))))

(defn next-primes [sieve candidate]
  (if (sieve candidate)
    (recur (next-sieve sieve candidate) (+ candidate 2))
    (cons candidate 
          (lazy-seq (next-primes (next-sieve sieve candidate) 
                                 (+ candidate 2))))))

