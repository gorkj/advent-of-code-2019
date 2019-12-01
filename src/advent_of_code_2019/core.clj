(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn calc-fuel
  "Calculate the amount of fuel required for a mass."
  [mass]
  (let [res (- (quot mass 3) 2)]
    (if (neg? res) 0 res))
  )

(defn calc-fuel-with-fuel
  "Calculate total fuel including fuel to carry the extra load of the fuel itself."
  [mass]
  (->>(iterate calc-fuel mass)
      (drop 1)
      (take-while #(not= 0 %))
      (reduce +)))

(defn -main
  "Runs all the problems sequentially"
  [& args]
  (print "Day 1a: ")
  (println (->> (io/resource "day-1.csv")
                io/reader
                line-seq
                (map #(Integer/parseInt %))
                (map calc-fuel)
                (reduce +)))
  (print "Day 1b: ")
  (println (->> (io/resource "day-1.csv")
                io/reader
                line-seq
                (map #(Integer/parseInt %))
                (map calc-fuel-with-fuel)
                (reduce +)))
  )
