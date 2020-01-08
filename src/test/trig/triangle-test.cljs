(ns trig.triangle-test
  (:require [cljs.test :refer (deftest is run-tests)]
            [trig.triangle :as tri]))

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(deftest solves-missing-angle
  (is (= 35 (:angle3 (tri/solve-triangle {:angle3 nil
                                          :angle1 40
                                          :angle2 105})))))

(deftest one-side-all-angles-test
  (is (= 5.27 (round (:line2 (tri/solve-triangle {:angle3 35
                                                  :angle1 40
                                                  :angle2 105
                                                  :line1  4.7})) 100))))

(comment
  (:angle3 @tri/triangle)
 (run-tests) )