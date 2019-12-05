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

(defn- calc-pos
  "Calculates new position"
  [[x y] [d c]]
  (case d
    "R" [(+ x c) y]
    "L" [(- x c) y]
    "U" [x (+ y c)]
    "D" [x (- y c)]))

(defn- calc-positions
  "Calculates all the new positions given a seq of movements"
  [prg]
  (reductions calc-pos [0 0] prg))

(defn- calc-borders
  "Finds the borders of a bonding rectangle given all the positions"
  [positions]
  [(apply min (map first positions))
   (apply max (map first positions))
   (apply min (map second positions))
   (apply max (map second positions))])

(comment
  (def prgstr "R8,U5,L5,D3")
  (def prgstr "U7,R6,D4,L4")
  (def prgstr (first (line-seq (io/reader (io/resource "day-3")))))

  (def d (let [steps (parse-prg prgstr)
               positions (calc-positions steps)
               [xmin xmax ymin ymax] (calc-borders positions)]
           {:steps steps
            :positions positions
            :bbox [xmin xmax ymin ymax]}))

  (let [[xmin xmax ymin ymax] (:bbox d)
        width (+ (Math/abs xmin) (Math/abs xmax))
        height (+ (Math/abs ymin) (Math/abs ymax))]
    [width height]))
