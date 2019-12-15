(ns advent-of-code-2019.wires
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.core.matrix :as ccm]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(defnp parse-prg
  "Splits instructions and converts count to integer"
  [prgstr]
  (map #(let [[_ dir cnt-str] %
              cnt (Integer/parseInt cnt-str)]
          [dir cnt])
       (re-seq #"([R|L|U|D])(\d+)" prgstr)
       ))

(defn- calc-positions
  "Calculates new positions"
  [prev [d c]]
  (let [[x y] (last prev)]
    (concat (drop-last prev)
            (case d
              "R" (for [i (range x (+ x c 1))] [i y])
              "L" (for [i (range x (- x c 1) -1)] [i y])
              "U" (for [j (range y (+ y c 1))] [x j])
              "D" (for [j (range y (- y c 1) -1)] [x j])))))

(defn- calc-all-positions
  "Calculates all the new positions given a seq of movements"
  [prg]
  (reduce calc-positions [[0 0]] prg))

(defn- dist
  [[a b]]
  (+ (Math/abs a) (Math/abs b)))

(defn- manhattan-distance [w1prg w2prg]
  (apply min
         (filter #(not (= 0 %)) ; origin is not valid
                 (map dist
                      (let [wire1 (into #{} (calc-all-positions (parse-prg w1prg)))
                            wire2 (into #{} (calc-all-positions (parse-prg w2prg)))]
                        (clojure.set/intersection wire1 wire2))))))


(defnp calc-positions-with-count
  "Calculates new positions"
  [prev [direction steps]]
  (let [[x y c] (last prev)
        c2 (atom (dec c))]
    (concat (drop-last prev)
            (case direction
              "R" (for [i (range x (+ x steps 1))] [i y (swap! c2 inc)])
              "L" (for [i (range x (- x steps 1) -1)] [i y (swap! c2 inc)])
              "U" (for [j (range y (+ y steps 1))] [x j (swap! c2 inc)])
              "D" (for [j (range y (- y steps 1) -1)] [x j (swap! c2 inc)])))))

(defnp calc-all-positions-with-count
  "Calculates all the new positions given a seq of movements"
  [prg]
  (reduce calc-positions-with-count [[0 0 0]] prg))

(defnp not-origin-point?
  [[x y]]
  (and (not= x 0) (not= y 0)))

(defnp matching-pos?
  [[a b] [c d _]]
  (and (= a c) (= b d)))

(defnp sum-count
  [[a b c] [_ _ d]]
  [a b (+ c d)])

(defnp least-steps
  [[a b c] [_ _ d]]
  [a b (min c d)])

(defnp calc-intersections
  [w1positions w2positions]
  (filter not-origin-point?
          (clojure.set/intersection (into #{} (map drop-last w1positions))
                                    (into #{} (map drop-last w2positions)))))

(defnp fewest-steps
  [w1prg w2prg]
  (let [wire1 (calc-all-positions-with-count (parse-prg w1prg))
        wire2 (calc-all-positions-with-count (parse-prg w2prg))
        intersections (calc-intersections wire1 wire2)]
    (reduce least-steps
            (map #(reduce sum-count %)
                 (vals (group-by (juxt first second)
                                 (for [intersection intersections
                                       pos (concat wire1 wire2)
                                       :when (matching-pos? intersection pos)]
                                   pos)))))))

(comment
  (tufte/add-basic-println-handler! {})
  (profile {} (fewest-steps "R8,U5,L5,D3"
                            "U7,R6,D4,L4")) ;; 30 steps

  (profile {} (fewest-steps "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                            "U62,R66,U55,R34,D71,R55,D58,R83")) ;; 610 steps
  (profile {} (fewest-steps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) ;; 410 steps

  (profile {} (let [lines (line-seq (io/reader (io/resource "day-3")))
                    w1prg (first lines)
                    w2prg (second lines)]
                (fewest-steps w1prg w2prg))))
