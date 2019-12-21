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

(defn- get-param [mode value memory]
  (if (= :position mode) (nth memory value) value))

(defn- get-address [mode value memory]
  (if (= :immediate mode) (nth memory value) value))

(defn- add [index memory]
  (let [[op-and-modes a b loc] (drop index memory)
        [op m1 m2 m3] (read-op op-and-modes)]
    (intcomp (+ index 4)
             (assoc memory
                    (get-address m3 loc memory)
                    (+ (get-param m1 a memory)
                       (get-param m2 b memory))))))

(defn- mul [index memory]
  (let [[op-and-modes a b loc] (drop index memory)
        [op m1 m2 m3] (read-op op-and-modes)]
    (intcomp (+ index 4)
             (assoc memory
                    (get-address m3 loc memory)
                    (* (get-param m1 a memory)
                       (get-param m2 b memory))))))

(defn- input [index memory]
  (let [[op-and-modes loc] (drop index memory)
        [op m1] (read-op op-and-modes)]
    (intcomp (+ index 2)
             (assoc memory
                    (get-address m1 loc memory)
                    (edn/read-string (read-line))))))

(defn- output [index memory]
  (let [[op-and-modes loc] (drop index memory)
        [op m1] (read-op op-and-modes)]
    (println (get memory (get-address m1 loc memory)))
    (intcomp (+ index 2) memory)))

(defn- read-op
  [number]
  (let [[m3 m2 m1 & op] (format "%05d" number)]
    [(Integer/parseInt (apply str op))
     (if (= \0 m1) :position :immediate)
     (if (= \0 m2) :position :immediate)
     (if (= \0 m3) :position :immediate)]))

(defn intcomp
  ([memory]
   (intcomp 0 memory))
  ([index memory]
   (let [[op] (read-op (get memory index))]
     (case op
       1 (add index memory)
       2 (mul index memory)
       3 (input index memory)
       4 (output index memory)
       99 memory))))

(comment
  (intcomp [1002 4 3 4 33])
  (intcomp (get-int-program)))

