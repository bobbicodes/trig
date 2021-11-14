(ns trig.triangle-test
  (:require [cljs.test :refer (deftest is run-tests)]
            [trig.math :refer [pi rad]]
            [trig.triangle :as tri]))

;; https://www.khanacademy.org/math/trigonometry/trigonometry-right-triangles/trig-solve-for-a-side/e/trigonometry_2

(deftest right-triangle-sides-test
  (is (= (tri/right-triangle-sides {:sides [4 nil nil], :angles [nil (/ pi 2) (rad 35)]})
         {:sides [4 5.7125920269684585 6.973787182484393], :angles [nil 1.5707963267948966 0.6108652381980153]}))
  (is (= (tri/right-triangle-sides {:sides [nil nil 5], :angles [nil (/ pi 2) (rad 35)]})
         {:sides [2.8678821817552302 nil 5], :angles [nil 1.5707963267948966 0.6108652381980153]}))
  (is (= (tri/right-triangle-sides {:sides [nil nil 5], :angles [(rad 70) (/ pi 2) nil]})
         {:sides [1.7101007166283442 nil 5], :angles [1.2217304763960306 1.5707963267948966 nil]}))
  (is (= (tri/right-triangle-sides {:sides [nil 7 nil], :angles [nil (/ pi 2) (rad 65)]})
         {:sides [15.01154844356691 7 16.56341108206749], :angles [nil 1.5707963267948966 1.1344640137963142]}))
  (is (= (tri/right-triangle-sides {:sides [2 nil nil], :angles [nil (/ pi 2) (rad 65)]})
         {:sides [2 0.9326153163099972 2.2067558379249834], :angles [nil 1.5707963267948966 1.1344640137963142]}))
  (is (= (tri/right-triangle-sides {:sides [nil 3 nil], :angles [nil (/ pi 2) (rad 65)]})
         {:sides [6.433520761528676 3 7.098604749457495], :angles [nil 1.5707963267948966 1.1344640137963142]}))
  (is (= (tri/right-triangle-sides {:sides [nil 3 nil], :angles [(rad 20) (/ pi 2) nil]})
         {:sides [8.242432258363868 3 8.771413200489262], :angles [0.3490658503988659 1.5707963267948966 nil]}))
  (is (= (tri/right-triangle-sides {:sides [nil nil 3], :angles [nil (/ pi 2) (rad 50)]})
         {:sides [2.298133329356934 nil 3], :angles [nil 1.5707963267948966 0.8726646259971648]}))
  (is (= (tri/right-triangle-sides {:sides [5 nil nil], :angles [(rad 25) (/ pi 2) nil]})
         {:sides [5 2.331538290774993 5.516889594812459], :angles [0.4363323129985824 1.5707963267948966 nil]})))

;; https://www.khanacademy.org/math/trigonometry/trigonometry-right-triangles/trig-solve-for-an-angle/e/solve-for-an-angle-in-a-right-triangle?modal=1

(deftest right-triangle-angles-test
  (is (= (tri/right-triangle-angles {:sides [nil 2 6], :angles [nil (/ pi 2) nil]})
         {:sides [nil 2 6], :angles [0.3398369094541219 1.5707963267948966 1.2309594173407747]}))
  (is (= (tri/right-triangle-angles {:sides [8 2 nil], :angles [nil (/ pi 2) nil]})
         {:sides [8 2 nil], :angles [0.24497866312686414 1.5707963267948966 1.3258176636680326]})))

(comment
  (run-tests)
  )

(deftest infer-angle-test
  (is (= (tri/infer-angle
          {:vertices ["R" "T" "S"]
           :sides [3 nil nil]
           :angles [nil (/ pi 2) (/ pi 4)]})
         {:vertices ["R" "T" "S"]
          :sides [3 nil nil]
          :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})))

(deftest isosceles-sides-test
  (is (= (tri/iso-sides
          {:vertices ["R" "T" "S"]
           :sides [3 nil nil]
           :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})
         {:vertices ["R" "T" "S"]
          :sides [3 3 nil]
          :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})))

(deftest hypotenuse-test
  (is (= (tri/hypotenuse
          {:vertices ["R" "T" "S"]
           :sides [3 3 nil]
           :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})
         {:vertices ["R" "T" "S"]
          :sides [3 3 4.242640687119285]
          :angles [(/ pi 4) (/ pi 2) (/ pi 4)]})))

(deftest trig-ratios-test
  (is (= (tri/trig-ratios {:sides [12 35 37]})
         {:sin {:angle1 [35 37], :angle3 [12 37]}, 
          :csc {:angle1 [37 35], :angle3 [37 12]}, 
          :cos {:angle1 [12 37], :angle3 [35 37]}, 
          :sec {:angle1 [37 12], :angle3 [37 35]}, 
          :tan {:angle1 [35 12], :angle3 [12 35]}, 
          :cot {:angle1 [12 35], :angle3 [35 12]}})))

(deftest two-angles-test
  (is (= {:sides [5 2.331538290774993 5.516889594812459]
          :angles [0.4363323129985824 1.5707963267948966 1.1344640137963142]}
         (tri/solve-triangle
          {:sides [5 nil nil]
           :angles [(rad 25) (/ pi 2) nil]}))))

#_(deftest one-side-all-angles-test
  (is (= 5.27 (round (:side2 (tri/solve-triangle {:angle3 35
                                                  :angle1 40
                                                  :angle2 105
                                                  :side1  4.7})) 100)))
  (is (= 36 (round (:side2 (tri/solve-triangle {:angle3 109
                                                :angle1 47
                                                :angle2 24
                                                :side1  46})) 1))))

;; https://www.khanacademy.org/math/trigonometry/unit-circle-trig-func/xfefa5515:special-trigonometric-values-in-the-first-quadrant/e/trig-values-of-6-4-and-3

{:vertices ["K" "J" "L"], :sides [nil nil 1], :angles [nil (/ pi 2) (/ pi 3)]}

(comment
 (run-tests)
  )