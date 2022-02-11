(ns trig.triangle
  (:require [reagent.core :as r]
            [trig.latex :as latex :refer [tex]]
            [trig.math :refer [pi sq sqrt rad deg sin cos tan asin acos atan csc sec cot]]
            [trig.editor :as editor :refer [update-editor! !tri eval-all]]
            [trig.law-of-sines :as los]
            [trig.uc :as uc]
            [trig.components :refer [input button right-angle-box selected-angle selected-side render-triangle]]
            [trig.sin :as sin]
            [clojure.string :as str]))

;; When representing a triangle, we will count the sides and angles
;; from the top left and follow the triangle counterclockwise down and around.
;; If a right triangle, we standardize the right angle to be the second angle
;; located on the bottom left, formed by sides 1 and 2,
;; opposite the hypotenuse which is side 3.

(defonce tri
  (r/atom {:vertices ["E" "F" "D"]
           :sides [6.1 nil 6.7]
           :angles [nil (/ pi 2) nil]}))

(defn right-triangle-sides
  "Takes a triangle with a right angle as its second angle and at least one side 
   and one other angle defined, and calculates the remaining sides based on the 
   trigonometric ratios. All angles are measured in radians."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (cond
    (and (pos? angle1) (pos? side1))
    (assoc triangle :sides [side1 (* side1 (tan angle1)) (/ side1 (cos angle1))])
    (and (pos? angle1) (pos? side2))
    (assoc triangle :sides [(/ side2 (tan angle1)) side2 (/ side2 (sin angle1))])
    (and (pos? angle1) (pos? side3))
    (assoc triangle :sides [(* side3 (cos angle1)) side2 side3])
    (and (pos? angle3) (pos? side1))
    (assoc triangle :sides [side1 (/ side1 (tan angle3)) (/ side1 (sin angle3))])
    (and (pos? angle3) (pos? side2))
    (assoc triangle :sides [(* side2 (tan angle3)) side2 (/ side2 (cos angle3))])
    (and (pos? angle3) (pos? side3))
    (assoc triangle :sides [(* side3 (sin angle3)) side2 side3])
    :else triangle))

(right-triangle-sides {:sides [nil nil 4], :angles [nil (/ pi 2) (rad 70)]})
(right-triangle-sides {:sides [nil nil 4], :angles [(rad 40) (/ pi 2) nil]})
(right-triangle-sides {:sides [nil 2 nil], :angles [(rad 25) (/ pi 2) nil]})
(right-triangle-sides {:sides [nil 3 nil], :angles [nil (/ pi 2) (rad 65)]})

(defn solve-sides
  "Given a triangle with at least one side and one angle defined,
   calculates the remaining sides based on the trigonometric ratios.
   All angles are measured in radians."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (cond
    (and (pos? angle1) (pos? side3))
    (assoc triangle :sides [(* side3 (sin angle1)) (* side3 (cos angle1)) side3])
    (and (pos? angle1) (pos? side2))
    (assoc triangle :sides [(* side2 (tan angle1)) side2 (/ side2 (cos angle1))])
    (and (pos? angle1) (pos? side1))
    (assoc triangle :sides [side1 (/ side1 (tan angle1)) (/ side1 (sin angle1))])
    (and (pos? angle2) (pos? side3))
    (assoc triangle :sides [(* side3 (cos angle2)) (* side3 (sin angle2)) side3])
    (and (pos? angle2) (pos? side2))
    (assoc triangle :sides [(/ side2 (tan angle2)) side2 (/ side2 (sin angle2))])
    (and (pos? angle2) (pos? side1))
    (assoc triangle :sides [side1 (* side1 (tan angle2)) (/ side1 (cos angle2))])
    :else triangle))

(defn infer-angle
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (if (= 2 (count (remove nil? (:angles triangle))))
    (cond
      (nil? angle1)
      (assoc triangle :angles [(- pi angle2 angle3)
                               angle2 angle3])
      (nil? angle2)
      (assoc triangle :angles [angle1 (- pi angle1 angle3) angle3])
      (nil? angle3)
      (assoc triangle :angles [angle1 angle2 (- pi angle2 angle1)])
      :else triangle)
    triangle))

(defn right-triangle-angles
  "When 2 sides of a right triangle are given,
   calculates an angle using the inverse trig functions.
   If 2 angles are defined, will calculate the third."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (cond
    (= 2 (count (filter pos? [angle1 angle2 angle3])))
    (infer-angle triangle)
    (and (pos? side1) (pos? side2))
    (-> triangle
        (assoc :angles [(atan (/ side2 side1))
                        angle2
                        (atan (/ side1 side2))]))
    (and (pos? side2) (pos? side3))
    (-> triangle
        (assoc :angles [(asin (/ side2 side3))
                        angle2
                        (acos (/ side2 side3))]))
    (and (pos? side1) (pos? side3))
    (-> triangle
        (assoc :angles [(acos (/ side1 side3))
                        angle2
                        (asin (/ side1 side3))]))
    :else triangle))

(map deg (:angles (right-triangle-angles {:sides [nil 4 9], :angles [nil (/ pi 2) nil]})))
(map deg (:angles (right-triangle-angles {:sides [8 7 nil], :angles [nil (/ pi 2) nil]})))
(map deg (:angles (right-triangle-angles {:sides [4 nil 7], :angles [nil (/ pi 2) nil]})))

(deg 1.3258176636680326)



(defn law-of-sines [{[side1 side2 side3] :sides
                     [angle1 angle2 angle3] :angles
                     :as triangle}]
    (cond
      (and (pos? angle1) (pos? angle2) (pos? side2))
      (assoc triangle :sides [side1 side2 (* side2 (/ (sin angle2) (sin angle1)))])
      (and (pos? angle1) (pos? angle2) (pos? side3))
      (assoc triangle :sides [side1 (* side3 (/ (sin angle1) (sin angle2))) side3])
      (and (pos? angle1) (pos? side1) (pos? side2))
      (assoc triangle :angles [angle1 angle2 (asin (* (/ side1 side2) (sin angle1)))])
      (and (pos? angle1) (pos? side2) (pos? side3))
      (assoc triangle :angles [angle1 (asin (* (/ side3 side2) (sin angle1))) angle3])
      (and (pos? angle1) (pos? angle3) (pos? side1))
      (assoc triangle :sides [side1 (* side1 (/ (sin angle1) (sin angle3))) side3])
      (and (pos? angle1) (pos? angle3) (pos? side2))
      (assoc triangle :angles [angle1 (* side2 (/ (sin angle3) (sin angle1))) angle3])
      (and (pos? angle1) (pos? angle3) (pos? side3))
      (assoc triangle :sides [side1 (* side3 (/ (sin angle1) (sin angle3))) side3])
      (and (pos? angle2) (pos? angle3) (pos? side1))
      (assoc triangle :sides [side1 side2 (* side1 (/ (sin angle2) (sin angle3)))])
      (and (pos? angle2) (pos? angle3) (pos? side3))
      (assoc triangle :sides [(* side3 (/ (sin angle3) (sin angle2))) side2 side3])
      (and (pos? angle2) (pos? side1) (pos? side3))
      (assoc triangle :angles [angle1 angle2 (asin (* (/ side1 side3) (sin angle2)))])
      (and (pos? angle2) (pos? side2) (pos? side3))
      (assoc triangle :angles [(asin (* (/ side2 side3) (sin angle2))) angle2 angle3])
      (and (pos? angle3) (pos? side1) (pos? side2))
      (assoc triangle :angles [(asin (/ (* side2 (sin angle3)) side1)) angle2 angle3])
      (and (pos? angle3) (pos? side1) (pos? side3))
      (assoc triangle :angles [angle1 (asin (* (/ side3 side1) (sin angle3))) angle3])
      :else triangle))

(defn loc-side
  "Use law of cosines to solve for a side, given the opposite angle and 2 other sides."
  [s1 s2 a]
  (.sqrt js/Math
         (- (+ (* s1 s1) (* s2 s2))
            (* 2 s1 s2 (cos a)))))

(defn loc-angle
  "Use law of cosines to solve for an angle, given an opposite and 2 adjacent sides."
  [a1 o a2]
  (acos (/ (+ (* a1 a1) (* a2 a2) (- (* o o)))
           (* 2 a1 a2))))

(defn law-of-cosines [{[side1 side2 side3] :sides
                       [angle1 angle2 angle3] :angles
                       :as triangle}]
    (cond
      (and (pos? angle1) (pos? side1) (pos? side3))
      (assoc triangle :sides [side1 (loc-side side3 side1 angle1) side3])
      (and (pos? angle2) (pos? side1) (pos? side2))
      (assoc triangle :sides [side1 side2 (loc-side side1 side2 angle2)])
      (and (pos? angle3) (pos? side2) (pos? side3))
      (assoc triangle :sides [(loc-side side2 side3 angle3) side2 side3])
      (and (pos? side1) (pos? side2) (pos? side3))
      (assoc triangle :angles [(loc-angle side1 side2 side3)
                               (loc-angle side2 side3 side1)
                               (loc-angle side2 side1 side3)])
      :else triangle))

(defn solve-triangle
  "If 2 angles defined, will calculate the 3rd.
   Applies Law of Sines and Law of Cosines,
   solving any sides/angles possible."
  [triangle]
      (-> triangle
          infer-angle
          law-of-sines
          law-of-cosines))

(defn dist [x1 y1 x2 y2]
  (.sqrt js/Math (+ (sq (- x2 x1))
                    (sq (- y2 y1)))))

(defn divisible? [n d]
  (= 0 (mod n d)))

(defn simplify-frac [n d]
  [n d])

;; check divisibility into the following numbers:

(for [n (range 2 (inc (min 26 10)))
      :when (even? n)]
  n)

;; we put the divisibility results in descending order to find
;; the largest common divisor.

(reverse (map #(divisible? 26 %) [2 4 6 8 10]))
(reverse (map #(divisible? 10 %) [2 4 6 8 10]))

;; Just kidding, we can use Euclid's algorithm:

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(gcd 26 10)
(mod 26 10)
;; I forgot that's what we needed. Been up aall night.
;; I'm going to commit this so there's a record of
;; what I was about to do.

(simplify-frac 26 10)

(defonce trig-fn (r/atom "\\sin"))

(defn trig-ratios 
  "Takes a triangle with a right angle as angle2
   and sides of known length. Outputs a map of 
   trig ratios for angle1 and angle3."
  [{[side1 side2 side3] :sides}]
  {:sin {:angle1 [side2 side3]
         :angle3 [side1 side3]}
   :csc {:angle1 [side3 side2]
         :angle3 [side3 side1]}
   :cos {:angle1 [side1 side3]
         :angle3 [side2 side3]}
   :sec {:angle1 [side3 side1]
         :angle3 [side3 side2]}
   :tan {:angle1 [side2 side1]
         :angle3 [side1 side2]}
   :cot {:angle1 [side1 side2]
         :angle3 [side2 side1]}})

(defn ratio
  "Renders the trig ratios for angle1 and angle3 for the
   currently selected trig function. Simplifies ratios."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices :as triangle}]
  (let [{[numer1 denom1] :angle1
         [numer3 denom3] :angle3}
        ((keyword (str/replace @trig-fn "\\" "")) (trig-ratios triangle))
        gcd1 (gcd numer1 denom1)
        gcd3 (gcd numer3 denom3)]
    [:div.flex-container
     [:div.flex-item (tex (str @trig-fn "(\\angle{" label1 "})=\\dfrac{" (/ numer1 gcd1) "}{" (/ denom1 gcd1) "}"))]
     [:div.flex-item (tex (str @trig-fn "(\\angle{" label3 "})=\\dfrac{" (/ numer3 gcd3) "}{" (/ denom3 gcd3) "}"))]]))

(defonce obtuse? (r/atom {:angle1 false :angle2 false :angle3 false}))

(defn angle-tex [f deg n1 d1 n2 d2]
  (tex (str f deg
            "\\degree)=\\dfrac{"
            n1 "}{" d1
            "}=\\dfrac{"
            n2 "}{" d2 "}")))

(defn angle-sin []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[side1 side2 side3] :sides
         [angle1 angle2 angle3] :angles
         [label1 label2 label3] :vertices} @tri]
    (angle-tex "\\sin(" degrees
               (cond (= @selected-angle 1)
                     (str label3 label2)
                     (= @selected-angle 3)
                     (str label2 label1)
                     :else (str label1 label2))
               (str label1 label3)
               (cond (= @selected-angle 1)
                     (or side2 (str label3 label2))
                     (= @selected-angle 3)
                     (str label2 label1)
                     :else side1)
               (or side3 (str label1 label3)))))

(defn angle-cos []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[side1 side2 side3] :sides
         [angle1 angle2 angle3] :angles
         [label1 label2 label3] :vertices} @tri]
    (angle-tex "\\cos(" degrees
               (cond (= @selected-angle 1)
                     (str label1 label2)
                     (= @selected-angle 3)
                     (str label2 label3)
                     :else (str label3 label2))
               (str label1 label3)
               (cond (= @selected-angle 1)
                     (or side1 (str label1 label2))
                     (= @selected-angle 3)
                     side2
                     :else (str label3 label2))
               (or side3 (str label1 label3)))))

(defn angle-tan []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[side1 side2 side3] :sides
         [angle1 angle2 angle3] :angles
         [label1 label2 label3] :vertices} @tri]
    (angle-tex "\\tan(" degrees
               (cond (= @selected-angle 1)
                     (str label3 label2)
                     (= @selected-angle 3)
                     (str label2 label1)
                     :else (str label1 label2))
               (cond (= @selected-angle 1)
                     (str label1 label2)
                     (= @selected-angle 3)
                     (str label2 label3)
                     :else (str label3 label2))
               (cond (= @selected-angle 1)
                     (or side2 (str label3 label2))
                     (= @selected-angle 3)
                     (str label2 label1)
                     :else side1)
               (cond (= @selected-angle 1)
                     (or side1 (str label1 label2))
                     (= @selected-angle 3) side2
                     :else (str label3 label2)))))

(defn angle-data []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])]
  [:div [:h3 "Angle "
         (tex (str (get-in @tri [:vertices (dec @selected-angle)]) "="
                   degrees "\\degree"))]
   [:p] [angle-sin]
   [:p] [angle-cos]
   [:p] [angle-tan]]))

(defn side-data []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[side1 side2 side3] :sides
         [angle1 angle2 angle3] :angles
         [label1 label2 label3] :vertices} @tri]
    [:div [:h3 "Line "
           (let [p1 (get-in @tri [:vertices (dec @selected-side)])
                 p2 (get-in @tri [:vertices (mod @selected-side 3)])]
             (tex (str p2 p1)))]
     (cond
       (= @selected-side 1)
       [:div
        (tex (str side3 "\\cdot\\sin(" angle3 "\\degree)")) [:p]
        (tex (str side2 "\\cdot\\tan(" angle3 "\\degree)")) [:p]
        (tex (str side3 "\\cdot\\cos(" angle1 "\\degree)")) [:p]
        (tex (str "\\dfrac{" side2 "}{\\tan(" angle1 "\\degree)}"))]
       (= @selected-side 2) nil
       (= @selected-side 3)
       [:div
        (tex (str "\\dfrac{" side2 "}{\\sin(" angle1 "\\degree)}")) [:p]
        (tex (str "\\dfrac{" side1 "}{\\cos(" angle1 "\\degree)}")) [:p]
        (tex (str "\\dfrac{" side1 "}{\\sin(" angle3 "\\degree)}")) [:p]
        (tex (str "\\dfrac{" side2 "}{\\cos(" angle3 "\\degree)}"))])]))

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(defn pi-frac [n] 
  (cond 
    (= n 1) 1
    (= n pi) (tex "\\pi")
    (> 0.000000000000001
       (.abs js/Math (- (round (/ pi n) 1) (/ pi n))))
    (tex (str "\\dfrac{\\pi}{" (round (/ pi n) 1) "}"))
    :else n))

(defn isosceles? [tri]
  (= #{1 2}
     (set (vals (dissoc (frequencies (:angles tri)) nil)))))

(defn iso-sides?
  "Returns true if the 2 opposite sides of the congruent
   angles of an isosceles triangle have both been defined."
  [triangle]
  (let [common-angle-rad
        (ffirst (filter #(= 2 (val %))
                        (frequencies (:angles triangle))))
        common-angles
        (for [angle (range 1 4)
              :when (= (get (:angles triangle) (dec angle))
                       common-angle-rad)]
          angle)
        length (apply max (for [side common-angles]
                            (get (:sides triangle) (dec side))))]
    (and (pos? length)
         (= (get (:sides triangle) (mod (first common-angles) 3))
            (get (:sides triangle) (mod (last common-angles) 3))))))

(defn iso-sides
  "Takes a triangle with 2 angles of a common length.
   If one of their opposite sides are defined,
   will fill in the other with the same value."
  [triangle]
  (let [common-angle-rad
        (ffirst (filter #(= 2 (val %))
                        (frequencies (:angles triangle))))
        common-angles
        (for [angle (range 1 4)
              :when (= (get (:angles triangle) (dec angle))
                       common-angle-rad)]
          angle)
        length (apply max (for [side common-angles]
                            (get (:sides triangle) (dec side))))]
    (-> triangle
        (assoc-in [:sides (mod (first common-angles) 3)] length)
        (assoc-in [:sides (mod (last common-angles) 3)] length))))

(defn iso [triangle]
  (let [common-angle-rad
        (ffirst (filter #(= 2 (val %)) (frequencies (:angles triangle))))
        common-angles
        (for [angle (range 1 4)
              :when (= (get (:angles triangle) (dec angle)) common-angle-rad)]
          angle)
        vertices (for [angle common-angles]
                   (get (:vertices triangle) (dec angle)))
        sides (for [side common-angles]
                (str (get (:vertices triangle) (mod side 3))
                     (get (:vertices triangle) (inc (mod side 3)))))
        length (apply max (for [side common-angles]
                            (get (:sides @tri) (dec side))))]
    [:div
     [:span "This triangle is isosceles because angles "
      (str (apply str (interpose " and " vertices))
           " both equal ")
      (pi-frac common-angle-rad) "."]
     [:p "The sides opposite the congruent angles in an isosceles triangle have equal lengths."] [:p]
     [:span 
      (str "So sides ") 
      (tex (str "\\overline{" (first sides) "}")) " and "
      (tex (str "\\overline{" (last sides) "}"))
      " both measure " length " units."]
     [button
      [:span "Compute sides"]
      #(do (swap! tri iso-sides)
           (update-editor! (str (iso-sides triangle))))]]))

(defn right?
  "Returns true if triangle has a right angle."
  [triangle]
  (contains? (set (:angles triangle)) 
             (/ pi 2)))

(defn hypotenuse
  "Calculates the 3rd side of a right triangle
   using the Pythagorean Theorem."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (let [right-angle
        (first (filter #(= (get (:angles triangle) %) (/ pi 2)) 
                       (range 2)))]
    (cond (= right-angle 1)
          (assoc-in triangle [:sides 2] 
                    (sqrt (+ (sq side1) (sq side2)))))))

(defn pythagoras?
  "Returns true if the triangle has 2 sides defined
   but not the hypotenuse, and thus could be solved
   using the Pythagorean Theorem."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
    (let [right-angle
        (first (filter #(= (get (:angles triangle) %) (/ pi 2)) 
                       (range 2)))]
      (cond (= right-angle 1)
            (and (not (pos? side3)) (pos? side1) (pos? side2)))))

(defn pythagoras1
  "Calculates the first side of a right triangle given
   the second and third sides using the Pythagorean Theorem."
  [{[side1 side2 side3] :sides
    :as triangle}]
  (assoc-in triangle [:sides 0] (sqrt (- (sq side3) (sq side2)))))

(pythagoras1 {:sides [nil 0.5 1]})

(- 244.13
   (* (- 244.13 35.435) (sqrt (- (sq 1) (sq 0.5)))))

(defonce deg-rad (r/atom "rad"))

(defn tri-angles
  [{[angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div.flex-container
   [:div.flex-item (tex (str "\\angle{" label1 "}=")) " "
    (pi-frac (if (= @deg-rad "deg") (deg angle1) angle1))] [:p]
   [:div.flex-item (tex (str "\\angle{" label2 "}=")) " "
    (pi-frac (if (= @deg-rad "deg") (deg angle2) angle2))] [:p]
   [:div.flex-item (tex (str "\\angle{" label3 "}=")) " "
    (pi-frac (if (= @deg-rad "deg") (deg angle3) angle3))] [:p]])

(defn tri-sides
  [{[side1 side2 side3] :sides
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div.flex-container
   [:div.flex-item (tex (str "\\overline{" label1 label2 "}=")) " "
    (pi-frac side1)] [:p]
   [:div.flex-item (tex (str "\\overline{" label2 label3 "}=")) " "
    (pi-frac side2)] [:p]
   [:div.flex-item (tex (str "\\overline{" label3 label1 "}=")) " "
    (tex (latex/sqrt-tex (pi-frac side3)))] [:p]])

(defn rad-deg-buttons []
  [:span
   [button "Rad" #(reset! deg-rad "rad")]
   [button "Deg" #(reset! deg-rad "deg")]])

(defn special-angles 
  "Checks if a triangle's angles share a simple relationship that 
   can simplify calculation, if so, displays a button that will
   swap the value of the triangle with the calculation performed."
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  (when (and (nil? side1)
             (contains? (set (:angles triangle)) (/ pi 6))
             (contains? (set (:angles triangle)) (/ pi 2)))
    [:div
     [:div "A " (tex "\\dfrac{\\pi}{6}-\\dfrac{\\pi}{3}-\\dfrac{\\pi}{2}") "triangle is half of an equilateral triangle."]
     [button
      [:span "Compute side"]
      #(do (swap! tri assoc-in [:sides 0] 0.5)
           (update-editor! (str @tri)))]])
  (when (and (nil? side2)
             (contains? (set (:angles triangle)) (/ pi 3))
             (contains? (set (:angles triangle)) (/ pi 2)))
    [:div
     [:div "A " (tex "\\dfrac{\\pi}{6}-\\dfrac{\\pi}{3}-\\dfrac{\\pi}{2}") "triangle is half of an equilateral triangle."]
     [button
      [:span "Compute side"]
      #(do (swap! tri assoc-in [:sides 1] 0.5)
           (update-editor! (str @tri)))]]))

(defn tri-data
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div
   [tri-angles triangle]
   [tri-sides triangle]
   (when (right? triangle)
     [:div
      [:p (str "This is a right triangle.")]])
   (when (= 2 (count (filter pos? [angle1 angle2 angle3])))
     [:div
      [:div "The interior angle measures of a triangle always add up to " (tex "\\pi") "."]
      [button
       [:span "Compute "
        (tex (str "\\angle{"
                  (cond (nil? angle1) label1
                        (nil? angle2) label2
                        (nil? angle3) label3) "}"))]
       #(do (swap! tri infer-angle)
            (update-editor! (str @tri)))] [:p]])
   (special-angles triangle)
   (when (and (isosceles? triangle)
              (not (iso-sides? triangle)))
     [iso triangle])
   (when (pythagoras? triangle)
     [:div
      [:p "The hypotenuse can be calculated by the Pythagorean Theorem."]
      [button
       "Calculate hypotenuse"
       #(do (swap! tri hypotenuse)
            (update-editor! (str @tri)))]])])

(defn ratio-buttons []
  (into [:span]
   (for [function ["\\sin" "\\csc" "\\cos" "\\sec" "\\tan" "\\cot"]]
     [button (tex function) #(reset! trig-fn function)])))

(defonce counter (r/atom 0))

(js/setInterval #(swap! counter inc) 10)

(defn abs [n] (.abs js/Math n))

(defn nearly-equal? [n1 n2]
  (> 0.0000001 (abs (- n1 n2))))

(defn frac 
  "Renders a fraction that may have a square root in the numerator.
   Forgives minor arithmetic discrepancies."
  [n]
  (or (last (first (filter #(nearly-equal? ((fn [[n _]] n) %) n)
                                (mapcat
                                 (fn [s]
                                   (map
                                    (fn [d]
                                      [(/ (sqrt s) d) (str "\\dfrac{\\sqrt" s "}{" d "}")])
                                    (range -12 11)))
                                 (range 2 11)))))
           n))

(defn app []
  (let [rotation-speed 0.001
        theta (mod (* rotation-speed @counter) (* pi 2))]
    [:div#app
     #_[editor/editor (str @tri) !tri {:eval? true}]
     #_[:button
        {:on-click
         #(reset! tri (eval-all
                       (str "(def pi js/Math.PI)
                           (defn rad [deg] (* deg (/ pi 180)))"
                            (some-> @!tri .-state .-doc str))))}
        "Eval"]
     #_[rad-deg-buttons]
     #_[button "Solve" #(do (swap! tri solve-triangle)
                            (update-editor! (str @tri)))]
    ; [ratio-buttons]
     
     [uc/uc-theta theta]
     [:div.flex-container
      [:div.flex-item (tex (str "\\theta=" (round theta 100)))]
      [:div.flex-item (tex (str "\\cos{\\theta=" (round (cos theta) 100) "}"))]
      [:div.flex-item (tex (str "\\sin{\\theta=" (round (sin theta) 100) "}"))]]
     ;(tex (frac (cos (rad 330))))
     ;[uc/uc-2 @tri]
     ;[tri-data @tri] [:p]
     ;[:p]
     ;[ratio @tri] [:p]
     ;[render-triangle @tri]
     #_[:div
        [angle-data]
        [side-data]]
      ;[los/law-of-sines "A" @tri]
     ]))
