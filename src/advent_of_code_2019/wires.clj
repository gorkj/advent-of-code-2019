(ns advent-of-code-2019.wires
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  )

(def arr2d (make-array Integer/TYPE 50 50))

(defn prnarr2d [arr2d]
  (doseq [row arr2d]
    (doseq [a row]
      (print (str a " ")))
    (println)
    ))

(prnarr2d arr2d)


