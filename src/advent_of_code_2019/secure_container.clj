(ns advent-of-code-2019.secure-container)

(defn- digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(def numbers [111111 111123 135679 223450 123789])

(defn- adjacent-present?
  [number]
  (some? (some (fn [[a b]] (= a b))
               (partition 2 1 (digits number)))))

;;(map adjacent-present? numbers)

(defn- adjacent-but-not-three?
  [number]
  (some? (some #(= 2 %) (map count (partition-by identity (digits number)))))
  )

;;(map adjacent-but-not-three? [112233 123444 111122])

(defn- never-decrese?
  [number]
  (every? (fn [[a b]] (<= a b))
          (partition 2 1 (digits number))))

;;(map never-decrese? numbers)

(filter (every-pred adjacent-present? never-decrese?) numbers)
;; => (111111 111123)

(count (filter (every-pred adjacent-present? never-decrese?) (range 254032 789861)))
;; => 1033


(count (filter (every-pred adjacent-but-not-three? never-decrese?) (range 254032 789861)))
;; => 670
