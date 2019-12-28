(ns advent-of-code-2019.intcode-mk2-test
  (:require [advent-of-code-2019.intcode-mk2 :refer [intcomp calc-amplification calc-max-settings]]
            [clojure.test :refer [deftest testing is]]))

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

(deftest serial-amplification
  (testing "single permutation"
    (is (= 43210 (calc-amplification [4 3 2 1 0] [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])))
    (is (= 54321 (calc-amplification [0,1,2,3,4] [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                                  101,5,23,23,1,24,23,23,4,23,99,0,0])))
    (is (= 65210 (calc-amplification [1,0,4,3,2] [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                                  1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]))))
  (testing "find-max-phase-settings"
    (is (= [[4 3 2 1 0] 43210] (calc-max-settings [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])))
    (is (= [[0,1,2,3,4] 54321] (calc-max-settings  [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                                                    101,5,23,23,1,24,23,23,4,23,99,0,0])))
    (is (= [[1,0,4,3,2] 65210] (calc-max-settings  [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                                    1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])))))
