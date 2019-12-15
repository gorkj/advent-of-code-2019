(ns advent-of-code-2019.intcode
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [advent-of-code-2019.misc :refer [digits]]
            [clojure.test :refer [deftest testing is]]))

(defn get-int-program []
  (mapv #(Integer/parseInt %)
        (str/split (slurp (io/resource "day-5")) #"," )))

(defn- add [index memory]
  (let [[op a b loc] (drop index memory)]
    (intcomp (+ index 4)
             (assoc memory loc (+ (nth memory a) (nth memory b))))))

(defn- mul [index memory]
  (let [[op a b loc] (drop index memory)]
    (intcomp (+ index 4)
             (assoc memory loc (* (nth memory a) (nth memory b))))))

(defn- input [index memory]
  (let [[op loc] (drop index memory)]
    (intcomp (+ index 2)
             (assoc memory loc (edn/read-string (read-line))))))

(defn- output [index memory]
  (let [[op loc] (drop index memory)]
    (println (get memory loc))
    (intcomp (+ index 2) memory)))

(digits 1001)

(defn- read-op
  [number]
  (let [[m3 m2 m1 & op] (format "%05d" number)]
    [(Integer/parseInt (apply str op))
     (if (= \0 m1) :pos :imm)
     (if (= \0 m2) :pos :imm)
     (if (= \0 m3) :pos :imm)]
    )
  )

(read-op 1001)

(defn intcomp
  ([memory]
   (intcomp 0 memory))
  ([index memory]
   (case (get memory index)
     1 (add index memory)
     2 (mul index memory)
     3 (input index memory)
     4 (output index memory)
     99 memory)))

(digits 1002)
(intcomp [3 0 4 0 99])

(take 20 (get-int-program))

(deftest day-2a-test
  (testing "Running programs"
    (is (= (intcomp [1,9,10,3,2,3,11,0,99,30,40,50]) [3500,9,10,70,2,3,11,0,99,30,40,50]))
    (is (= (intcomp [1,0,0,0,99])  [2,0,0,0,99]))
    (is (= (intcomp [2,3,0,3,99]) [2,3,0,6,99]))
    (is (= (intcomp [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
    (is (= (intcomp [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))))
