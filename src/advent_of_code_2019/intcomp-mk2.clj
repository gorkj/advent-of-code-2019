(ns advent-of-code-2019.intcode-mk2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [taoensso.timbre :refer [spy info]]
            [taoensso.truss :refer [have]]
            [advent-of-code-2019.misc :refer [digits]]
            [clojure.test :refer [deftest testing is]]))

(defn get-int-program []
  (mapv #(Integer/parseInt %)
        (str/split (slurp (io/resource "day-5")) #"," )))

(declare intcomp-int)

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

(defn- add [in out params address index memory]
  (intcomp-int in out (+ index 4)
           (assoc memory
                  address
                  (+ (first params) (second params)))))

(defn- mul [in out params address index memory]
  (intcomp-int in out (+ index 4)
           (assoc memory
                  address
                  (* (first params) (second params)))))

(defn- input [in out address index memory]
  (intcomp-int (rest in) out (+ index 2)
               (assoc memory
                      address
                      (first in))))

(defn- output [in out params index memory]
  ;;(info "output" (first params) index)
  #_(println (first params))
  (swap! out conj (first params))
  (intcomp-int in out (+ index 2) memory))

(defn- jump-if-true [in out params index memory]
  (intcomp-int in out (if (not= 0 (first params))
                    (second params)
                    (+ index 3))
           memory))

(defn- jump-if-false [in out params index memory]
  (intcomp-int in out (if (= 0 (first params))
                    (second params)
                    (+ index 3))
           memory))

(defn- less-than [in out params address index memory]
  (intcomp-int in out (+ index 4)
           (assoc memory
                  address
                  (if (< (first params) (second params)) 1 0))))
(defn- equals [in out params address index memory]
  (intcomp-int in out (+ index 4)
           (assoc memory
                  address
                  (if (= (first params) (second params)) 1 0))))

(defn intcomp-int
  ([in out memory]
   (intcomp-int in out 0 memory))
  ([in out index memory]
   (let [[op params address] (read-op index memory)]
     (case op
       1 (add in out params address index memory)
       2 (mul in out params address index memory)
       3 (input in out address index memory)
       4 (output in out params index memory)
       5 (jump-if-true in out params index memory)
       6 (jump-if-false in out params index memory)
       7 (less-than in out params address index memory)
       8 (equals in out params address index memory)
       99 memory)
     )))

(defn intcomp
  [in memory]
  (let [out (atom [])]
    (intcomp-int in out memory)
    (first @out)))

(deftest intcomp-with-automatic-io
  (testing "compare to eight"
    (testing "position mode"
      (let [prg [3,9,8,9,10,9,4,9,99,-1,8]]
        (is (= 0 (intcomp [7] prg)))
        (is (= 1 (intcomp [8] prg)))
        (is (= 0 (intcomp [9] prg))))
      (let [prg [3,9,7,9,10,9,4,9,99,-1,8]]
        (is (= 1 (intcomp [7] prg)))
        (is (= 0 (intcomp [8] prg)))))
    (testing "immediate mode"
      (let [prg [3,3,1108,-1,8,3,4,3,99]]
        (is (= 0 (intcomp [7] prg)))
        (is (= 1 (intcomp [8] prg)))
        (is (= 0 (intcomp [9] prg))))
      (let [prg [3,3,1107,-1,8,3,4,3,99]]
        (is (= 1 (intcomp [7] prg)))
        (is (= 0 (intcomp [8] prg))))))
  (testing "zero or not"
    (let [prg [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]
      (is (= 0 (intcomp [0] prg)))
      (is (= 1 (intcomp [1] prg)))
      (is (= 1 (intcomp [42] prg))))
    (let [prg [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]
      (is (= 0 (intcomp [0] prg)))
      (is (= 1 (intcomp [1] prg)))
      (is (= 1 (intcomp [42] prg))))
    ))

(deftest jumping
  (testing "Jump program"
    (let [prg [3 21,1008 21 8 20,1005 20 22,107 8 21 20,1006 20 31,1106 0 36,98 0 0,1002 21 125 20,
               4 20,1105 1 46,104 999,1105 1 46,1101 1000 1 20,4 20,1105 1 46 98,99]]
      (is (= 999 (intcomp [7] prg )))
      (is (= 1000 (intcomp [8] prg )))
      (is (= 1001 (intcomp [9] prg ))))))

(let [prg [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
      phases [4 3 2 1 0]]
  (map #(intcomp % prg))
  )
;; 43210


