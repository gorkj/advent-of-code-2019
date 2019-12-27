(ns advent-of-code-2019.orbits
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [loom.graph :refer [graph digraph nodes predecessors]]
            [loom.alg :refer [shortest-path]]
            [loom.io :as lio]))

(def example-data "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(def example-data-2 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(defn parse-data
  [data]
   (for [[_ a b] (re-seq #"(\w+)\)(\w+)" data)]
     [a b]))

(defn count-all-orbits
  [input]
  (let [g (apply digraph input)
        nodes (nodes g)
        root (filter #(= 0 (count (predecessors g %))) )]
    (reduce +
            (for [node nodes]
              (loop [cur node
                     acc 0]
                (if (= cur "COM")
                  acc
                  (recur (first (predecessors g cur)) (inc acc))))))))

(defn count-transfers-required
  [input]
  (let [g (apply graph input)]
    ;;(lio/view g)
    (- (count
        (shortest-path g "YOU" "SAN")) 3)))

#_(-> (parse-data example-data)
    count-all-orbits)

#_(-> (parse-data example-data-2)
    count-transfers-required
    )

#_(-> (slurp (io/resource "orbits.txt"))
    parse-data
    count-all-orbits)
;; => 261306

#_(-> (slurp (io/resource "orbits.txt"))
      parse-data
      count-transfers-required)
;; => 382
