(ns trig.triangle-test
  (:require [cljs.test :refer (deftest is run-tests)]
            [trig.triangle :as tri]))

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(def pi js/Math.PI)

(deftest infer-angle-test
  (is (= (tri/infer-angle-rad
          {:vertices ["R" "T" "S"]
           :lines [3 nil nil]
           :angles [nil (/ pi 2) (/ pi 4)]})
         {:vertices ["R" "T" "S"]
          :lines [3 nil nil]
          :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})))

(deftest isosceles-sides-test
  (is (= (tri/iso-sides
          {:vertices ["R" "T" "S"]
           :lines [3 nil nil]
           :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})
         {:vertices ["R" "T" "S"]
          :lines [3 3 nil]
          :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})))

(deftest hypotenuse-test
  (is (= (tri/hypotenuse
          {:vertices ["R" "T" "S"]
           :lines [3 3 nil]
           :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})
         {:vertices ["R" "T" "S"]
          :lines [3 3 4.242640687119285]
          :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})))

#_(deftest two-angles-test
  (is (= 35 (:angle3 (tri/solve-triangle {:angle1 40
                                          :angle2 105})))))

#_(deftest one-side-all-angles-test
  (is (= 5.27 (round (:line2 (tri/solve-triangle {:angle3 35
                                                  :angle1 40
                                                  :angle2 105
                                                  :line1  4.7})) 100)))
  (is (= 36 (round (:line2 (tri/solve-triangle {:angle3 109
                                                :angle1 47
                                                :angle2 24
                                                :line1  46})) 1))))

#_(deftest two-sides-one-angle-test
  (is (= 184 (round (:line1 (tri/solve-triangle {:angle3 3
                                                 :line2  915
                                                 :line3  736})) 1)))
  (is (= 262 (round (:line2 (tri/solve-triangle {:line3  250
                                                 :angle1 56
                                                 :line1  300})) 1))))

(comment
 (run-tests) 
  )