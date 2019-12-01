(ns advent-of-code-2019.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.core :refer :all]))

(deftest day-1-test
  (testing "Day 1 Fuel consumption"
    (is (= (calc-fuel 12) 2))
    (is (= (calc-fuel 14) 2))
    (is (= (calc-fuel 1969) 654))
    (is (= (calc-fuel 100756) 33583))))
