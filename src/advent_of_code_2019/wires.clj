(ns advent-of-code-2019.wires
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  )

(def state {:arr (vec (repeat 10 (vec (repeat 11 0))))
            :x 0
            :y 0})

(defn pos [state x y]
  (-> state
      (assoc :x x)
      (assoc :y y)))

(defn view [state]
  (println)
  (let [arr (:arr state)
        xpos (:x state)
        ypos (:y state)]
    (doseq [[y row] (map-indexed vector arr)]
      (doseq [[x value] (map-indexed vector row)]
        (let [c (if (and (= x xpos) (= y ypos)) "X"
                    (if (zero? value) "." value))]
          (print (str c " ")))
        )
      (println))))

(defn m
  ([state num dir cnt]
   (if (zero? cnt)
     state
     (recur (m state num dir) num dir (dec cnt))))
  ([state num dir]
   (let [{:keys [x y]} state]
     (-> (case dir
           :up (assoc state :y (dec y))
           :right (assoc state :x (inc x))
           :down (assoc state :y (inc y))
           :left (assoc state :x (dec x)))
         (update-in [:arr y x] #(+ % num))
         )
     )))


(def prg1 ["R8" "U5" "L5" "D3"])
(def prg2 ["U7","R6","D4","L4"])

(comment
  (-> state
      (pos 1 8)
      (m 1 :right 8)
      (m 1 :up 5)
      (m 1 :left 5)
      (m 1 :down 3)
      (pos 1 8)
      (m 2 :up 7)
      (m 2 :right 6)
      (m 2 :down 4)
      (m 2 :left 4)
      (view)
      ))
