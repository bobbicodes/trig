(ns trig.triangle-test
  (:require [cljs.test :refer (deftest is run-tests)]
            [trig.triangle :as tri]))

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(def pi js/Math.PI)

(defn rad [deg] (* deg (/ pi 180)))

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

(deftest trig-ratios-test
  (is (= (tri/trig-ratios {:lines [12 35 37]})
         {:sin {:angle1 [35 37], :angle3 [12 37]}, 
          :csc {:angle1 [37 35], :angle3 [37 12]}, 
          :cos {:angle1 [12 37], :angle3 [35 37]}, 
          :sec {:angle1 [37 12], :angle3 [37 35]}, 
          :tan {:angle1 [35 12], :angle3 [12 35]}, 
          :cot {:angle1 [12 35], :angle3 [35 12]}})))

(deftest two-angles-test
  (is (= {:lines [5 2.331538290774993 5.516889594812459]
          :angles [0.4363323129985824 1.5707963267948966 1.1344640137963142]}
         (tri/solve-triangle
          {:lines [5 nil nil]
           :angles [(rad 25) (/ pi 2) nil]}))))

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