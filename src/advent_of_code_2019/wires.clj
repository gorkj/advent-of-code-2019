(ns advent-of-code-2019.wires
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.core.matrix :as ccm]))

(defn- parse-prg
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

(defn- calc-borders
  "Finds the borders of a bonding rectangle given all the positions"
  [positions]
  [(apply min (map first positions))
   (apply max (map first positions))
   (apply min (map second positions))
   (apply max (map second positions))])

(defn- dist
  [[a b]]
  (+ (Math/abs a) (Math/abs b)))

(defn manhattan-distance [w1prg w2prg]
  (apply min
         (filter #(not (= 0 %)) ; origin is not valid
                 (map dist
                      (let [wire1 (into #{} (calc-all-positions (parse-prg w1prg)))
                            wire2 (into #{} (calc-all-positions (parse-prg w2prg)))]
                        (clojure.set/intersection wire1 wire2))))))


#_(let [lines (line-seq (io/reader (io/resource "day-3")))
      w1prg (first lines)
      w2prg (second lines)]
  (manhattan-distance w1prg w2prg))

