(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
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


(defn get-int-program
  []
  (mapv #(Integer/parseInt %)
        (str/split (slurp (io/resource "day-2")) #"," ))
  )
(defn intcomp
  "Run program specified in memory"
  ([memory]
   (intcomp 0 memory))
  ([index memory]
   (if (< index (quot (count memory) 4))
     (let [[op x y loc] (nth (partition 4 memory) index)]
       (recur (inc index) (case op
                            1 (assoc memory loc (+ (nth memory x) (nth memory y)))
                            2 (assoc memory loc (* (nth memory x) (nth memory y)))
                            99 memory
                            "error")))
     memory)))

(defn intcomp2 [memory noun verb]
  "Run program with specified parameters"
  (-> (get-int-program)
      (assoc 1 noun)
      (assoc 2 verb)
      (intcomp)))

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
  (print "Day 2a: ")
  (println
   (first
    (-> (get-int-program)
        (assoc 1 12)
        (assoc 2 2)
        (intcomp)
        )))
  (print "Day 2b: ")
  (println
   (first
    (let [memory (get-int-program)]
      (for [x (range 100)
            y (range 100)
            :let [r (first (intcomp2 memory x y))]
            :when (= r 19690720)]
        [x y]))))
  )
