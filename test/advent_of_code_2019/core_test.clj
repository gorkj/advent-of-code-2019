(ns advent-of-code-2019.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.core :refer :all]))

(deftest day-1a-test
  (testing "Day 1 Fuel consumption"
    (is (= (calc-fuel 12) 2))
    (is (= (calc-fuel 14) 2))
    (is (= (calc-fuel 1969) 654))
    (is (= (calc-fuel 100756) 33583))))

(deftest day-1b-test
  (testing "Day 1 Fuel consumption including mass of fuel"
    (is (= (calc-fuel-with-fuel 14) 2))
    (is (= (calc-fuel-with-fuel 1969) 966))
    (is (= (calc-fuel-with-fuel 100756) 50346))))

(deftest day-2a-test
  (testing "Running programs"
    (is (= (intcomp [1,9,10,3,2,3,11,0,99,30,40,50]) [3500,9,10,70,2,3,11,0,99,30,40,50]))
    (is (= (intcomp [1,0,0,0,99])  [2,0,0,0,99]))
    (is (= (intcomp [2,3,0,3,99]) [2,3,0,6,99]))
    (is (= (intcomp [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
    (is (= (intcomp [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))))
