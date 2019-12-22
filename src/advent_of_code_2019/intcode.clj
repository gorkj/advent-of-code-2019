(ns advent-of-code-2019.intcode
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [advent-of-code-2019.misc :refer [digits]]))

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

(defn- oplen
  "Length of operations with parameters"
  [op]
  (case op 1 4,2 4,3 2,4 2,5 3,6 3,7 4,8 4,99 0))

(defn- read-param [value mode memory]
  (if (= :position mode) (nth memory value) value))

(defn- read-op
  [index memory]
  (let [prg (drop index memory)
        [op & modes] (parse-modes (first prg))
        [_ & params-raw] (take (oplen op) prg)
        params (map #(read-param %1 %2 memory) params-raw modes)
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

(defn- jump-if-true [params index memory]
  (intcomp (if (not= 0 (first params))
             (second params)
             (+ index 3))
           memory))

(defn- jump-if-false [params index memory]
  (intcomp (if (= 0 (first params))
             (second params)
             (+ index 3))
           memory))

(defn- less-than [params address index memory]
  (intcomp (+ index 4)
           (assoc memory
                  address
                  (if (< (first params) (second params)) 1 0))))
(defn- equals [params address index memory]
  (intcomp (+ index 4)
           (assoc memory
                  address
                  (if (= (first params) (second params)) 1 0))))

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
       5 (jump-if-true params index memory)
       6 (jump-if-false params index memory)
       7 (less-than params address index memory)
       8 (equals params address index memory)
       99 memory))))


(comment
  (intcomp [1002 4 3 4 33])

  (intcomp [3,9,8,9,10,9,4,9,99,-1,8])
  (intcomp [3,9,7,9,10,9,4,9,99,-1,8])
  (intcomp [3,3,1108,-1,8,3,4,3,99])
  (intcomp [3,3,1107,-1,8,3,4,3,99])

  (intcomp [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9])
  (intcomp [3,3,1105,-1,9,1101,0,0,12,4,12,99,1])
  (intcomp [3,21,
            1008,21,8,20,
            1005,20,22,
            107,8,21,20,
            1006,20,31,
            1106,0,36,
            98,0,0, ; 19 20 21
            1002,21,125,20,
            4,20,
            1105,1,46,
            104,999,
            1105,1,46,
            1101,1000,1,20,
            4,20,
            1105,1,46,98,
            99])

  (intcomp (get-int-program)))
