(ns advent-of-code-2019.intcode
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [advent-of-code-2019.misc :refer [digits]]
            [clojure.test :refer [deftest testing is]]))

(defn get-int-program []
  (mapv #(Integer/parseInt %)
        (str/split (slurp (io/resource "day-5")) #"," )))

(declare intcomp)

(defn- parse-modes
  [number]
  (let [[m3 m2 m1 & op] (format "%05d" number)]
    [(Integer/parseInt (apply str op))
     (if (= \0 m1) :position :immediate)
     (if (= \0 m2) :position :immediate)
     (if (= \0 m3) :position :immediate)]))

(defn- oplen [op]
  (case op 1 4 2 4 3 2 4 2 5 2 6 2 7 3 8 3 99 0))

(defn- read-param [value mode memory]
  (if (= :position mode) (nth memory value) value))

(defn- read-op
  [index memory]
  (let [prg (drop index memory)
        [op & modes] (parse-modes (first prg))
        [_ & params-raw] (take (oplen op) prg)
        params (map #(read-param %1 %2 memory) (drop-last params-raw) modes)
        address (last params-raw)]
    [op params address]))

(defn- add [params address index memory]
  (intcomp (+ index 4)
           (assoc memory
                  address
                  (+ (first params) (second params)))))

(defn- mul [params address index memory]
  (intcomp (+ index 4)
           (assoc memory
                  address
                  (* (first params) (second params)))))

(defn- input [address index memory]
  (intcomp (+ index 2)
           (assoc memory
                  address
                  (edn/read-string (read-line)))))

(defn- output [address index memory]
  (println (get memory address))
  (intcomp (+ index 2) memory))

(defn intcomp
  ([memory]
   (intcomp 0 memory))
  ([index memory]
   (let [[op params address] (read-op index memory)]
     (case op
       1 (add params address index memory)
       2 (mul params address index memory)
       3 (input address index memory)
       4 (output address index memory)
       99 memory))))

(comment
  (intcomp [1002 4 3 4 33])
  (intcomp (get-int-program)))
