(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn calc-fuel
  [mass]
  (- (quot mass 3) 2)
  )

(defn -main
  "Runs all the problems sequentially"
  [& args]
  (print "Day 1a: ")
  (println
   (->> (io/resource "day-1.csv")
        io/reader
        line-seq
        (map #(Integer/parseInt %))
        (map calc-fuel)
        (reduce +)
        )))
