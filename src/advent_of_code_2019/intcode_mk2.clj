(ns advent-of-code-2019.intcode-mk2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.math.combinatorics :as comb]
            [taoensso.timbre :refer [spy info]]
            [taoensso.truss :refer [have have? have!]]
            [advent-of-code-2019.misc :refer [digits]]
            [clojure.test :refer [deftest testing is]]))

(defn get-int-program [name]
  (mapv #(Integer/parseInt %)
        (str/split (slurp (io/resource name)) #"," )))

(declare intcomp-int)

(defn- parse-modes
  [number]
  (let [[m3 m2 m1 & op] (format "%05d" (have pos? number))]
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
    (have pos-int? op)
    (have #(not-any? nil? %) params)
    [op params address]))

(defn- add [in out params address index memory]
  (intcomp-int in out (+ index 4)
           (assoc memory
                  address
                  (+ (have number? (first params))
                     (have number? (second params))))))

(defn- mul [in out params address index memory]
  (intcomp-int in out (+ index 4)
           (assoc memory
                  address
                  (* (have number? (first params) :data params)
                     (have number? (second params) :data params)))))

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

#_(intcomp [9] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
              27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

(defn calc-amplification
  [phases prg]
  (loop [p phases
         in 0]
    (if (empty? p)
      in
      (recur (rest p) (intcomp [(first p) in] prg)))))

(defn calc-max-settings
  [prg]
  (apply max-key
         second
         (map #(list % (calc-amplification % prg)) (comb/permutations (range 5)))))

(defn calc-feedback-amplification
  [phases prg]
  nil
  )

;;(comb/permutations (range 5 10))



#_(calc-amplification [9,8,7,6,5] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])


(deftest feedback-loop-amplification
  (testing "small"
    (is (= 139629729 (calc-feedback-amplification [9,8,7,6,5] [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                                                               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])))
    (is (= 18216 (calc-feedback-amplification [9,7,8,5,6] [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                                           -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                                           53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])))
    ))

;;(calc-max-settings (get-int-program "amp-program"))
;; => ([1 3 2 4 0] 34852)
