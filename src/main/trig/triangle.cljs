(ns trig.triangle
  (:require [reagent.core :as r]
            [trig.latex :as latex :refer [tex]]
            [trig.math :refer [pi sq sqrt sin cos tan asin acos atan csc sec cot]]
            [trig.editor :as editor :refer [update-editor! !tri eval-all]]
            [sci.core :as sci]
            [trig.law-of-sines :as los]
            [trig.uc :as uc]
            [trig.components :refer [input button right-angle-box selected-angle selected-line render-triangle]]
            [trig.sin :as sin]
            [cljs.spec.alpha :as s]
            [clojure.string :as str]))

(defonce tri
  (r/atom {:vertices ["A" "B" "C"]
           :lines [9 11 15]
           :angles [0 81 0]}))

(defn solve-sides
  "Given a triangle with at least one side and one angle defined,
   calculates the remaining sides based on the trigonometric ratios.
   All angles are measured in radians."
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (cond
    (and (pos? angle1) (pos? line3))
    (assoc triangle :lines [(* line3 (sin angle1))
                            (* line3 (cos angle1))
                            line3])
    (and (pos? angle1) (pos? line2))
    (assoc triangle :lines [(* line2 (tan angle1))
                            line2
                            (/ line2 (cos angle1))])
    (and (pos? angle1) (pos? line1))
    (assoc triangle :lines [line1
                            (/ line1 (tan angle1))
                            (/ line1 (sin angle1))])
    (and (pos? (:degrees angle2)) (pos? line3))
    (assoc triangle :lines [(* line3 (cos angle2))
                            (* line3 (sin angle2))
                            line3])
    (and (pos? (:degrees angle2)) (pos? line2))
    (assoc triangle :lines [(/ line2 (tan angle2))
                            line2
                            (/ line2 (sin angle2))])
    (and (pos? angle2) (pos? line1))
    (assoc triangle :lines [line1
                            (* line1 (tan angle2))
                            (/ line1 (cos angle2))])
    :else triangle))

(defn infer-angle [{[line1 line2 line3] :lines
                    [angle1 angle2 angle3] :angles
                    [label1 label2 label3] :vertices
                    :as triangle}]
   (cond
     (nil? angle1)
     (assoc triangle :angles [(- 180 angle2 angle3)
                              angle2 angle3])
     (nil? angle2)
     (assoc triangle :angles [angle1 (- 180 angle1 angle3) angle3])
     (nil? angle3)
     (assoc triangle :angles [angle1 angle2 (- 180 angle2 angle1)])))

(defn solve-angles
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  (cond
    (= 2 (count (filter pos? [angle1 angle2 angle3])))
    (infer-angle triangle)
    (and (pos? line1) (pos? line3))
    (-> triangle
        (assoc :angles [(asin (/ line1 line3))
                        (acos (/ line1 line3))
                        angle3]))
    (and (pos? line2) (pos? line3))
    (-> triangle
        (assoc :angles [(acos (/ line2 line3))
                        (asin (/ line2 line3))
                        angle3]))
    (and (pos? line2) (pos? line1))
    (-> triangle
        (assoc :angles [(atan (/ line1 line2))
                        (atan (/ line2 line1))
                        angle3]))
    :else triangle))

(defn infer-angle-rad
  [{[line1 line2 line3] :lines
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

(defn law-of-sines [{[line1 line2 line3] :lines
                     [angle1 angle2 angle3] :angles
                     :as triangle}]
    (cond
      (and (pos? angle1) (pos? angle2) (pos? line2))
      (assoc triangle :lines [line1 line2 (* line2 (/ (sin angle2) (sin angle1)))])
      (and (pos? angle1) (pos? angle2) (pos? line3))
      (assoc triangle :lines [line1 (* line3 (/ (sin angle1) (sin angle2))) line3])
      (and (pos? angle1) (pos? line1) (pos? line2))
      (assoc triangle :angles [angle1 angle2 (asin (* (/ line1 line2) (sin angle1)))])
      (and (pos? angle1) (pos? line2) (pos? line3))
      (assoc triangle :angles [angle1 (asin (* (/ line3 line2) (sin angle1))) angle3])
      (and (pos? angle1) (pos? angle3) (pos? line1))
      (assoc triangle :lines [line1 (* line1 (/ (sin angle1) (sin angle3))) line3])
      (and (pos? angle1) (pos? angle3) (pos? line2))
      (assoc triangle :angles [angle1 (* line2 (/ (sin angle3) (sin angle1))) angle3])
      (and (pos? angle1) (pos? angle3) (pos? line3))
      (assoc triangle :lines [line1 (* line3 (/ (sin angle1) (sin angle3))) line3])
      (and (pos? angle2) (pos? angle3) (pos? line1))
      (assoc triangle :lines [line1 line2 (* line1 (/ (sin angle2) (sin angle3)))])
      (and (pos? angle2) (pos? angle3) (pos? line3))
      (assoc triangle :lines [(* line3 (/ (sin angle3) (sin angle2))) line2 line3])
      (and (pos? angle2) (pos? line1) (pos? line3))
      (assoc triangle :angles [angle1 angle2 (asin (* (/ line1 line3) (sin angle2)))])
      (and (pos? angle2) (pos? line2) (pos? line3))
      (assoc triangle :angles [(asin (* (/ line2 line3) (sin angle2))) angle2 angle3])
      (and (pos? angle3) (pos? line1) (pos? line2))
      (assoc triangle :angles [(asin (/ (* line2 (sin angle3)) line1)) angle2 angle3])
      (and (pos? angle3) (pos? line1) (pos? line3))
      (assoc triangle :angles [angle1 (asin (* (/ line3 line1) (sin angle3))) angle3])
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

(defn law-of-cosines [{[line1 line2 line3] :lines
                       [angle1 angle2 angle3] :angles
                       :as triangle}]
    (cond
      (and (pos? angle1) (pos? line1) (pos? line3))
      (assoc triangle :lines [line1 (loc-side line3 line1 angle1) line3])
      (and (pos? angle2) (pos? line1) (pos? line2))
      (assoc triangle :lines [line1 line2 (loc-side line1 line2 angle2)])
      (and (pos? angle3) (pos? line2) (pos? line3))
      (assoc triangle :lines [(loc-side line2 line3 angle3) line2 line3])
      (and (pos? line1) (pos? line2) (pos? line3))
      (assoc triangle :angles [(loc-angle line1 line2 line3)
                               (loc-angle line2 line3 line1)
                               (loc-angle line2 line1 line3)])
      :else triangle))

(defn solve-triangle
  "If 2 angles defined, will calculate the 3rd.
   Applies Law of Sines and Law of Cosines,
   solving any sides/angles possible."
  [triangle]
      (-> triangle
          infer-angle-rad
          law-of-sines
          law-of-cosines))

(infer-angle-rad @tri)

(defn rad [deg] (* deg (/ pi 180)))

(defn deg [rad] (/ rad (/ pi 180)))

(solve-triangle {:angles [(rad 40)
                          (rad 105)
                          nil]})

(cos (/ pi 4))
(/ 3 4.242640687119285)
(defn dist [x1 y1 x2 y2]
  (.sqrt js/Math (+ (sq (- x2 x1))
                    (sq (- y2 y1)))))

(defonce trig-fn (r/atom "\\sin"))

(defn trig-ratios 
  "Takes a triangle with a right angle as angle2
   and sides of known length. Outputs a map of 
   trig ratios for angle1 and angle3."
  [{[line1 line2 line3] :lines}]
  {:sin {:angle1 [line2 line3]
         :angle3 [line1 line3]}
   :csc {:angle1 [line3 line2]
         :angle3 [line3 line1]}
   :cos {:angle1 [line1 line3]
         :angle3 [line2 line3]}
   :sec {:angle1 [line3 line1]
         :angle3 [line3 line2]}
   :tan {:angle1 [line2 line1]
         :angle3 [line1 line2]}
   :cot {:angle1 [line1 line2]
         :angle3 [line2 line1]}})

@tri
(trig-ratios {:vertices ["B" "C" "A"], :lines [3 4 5], :angles [nil 1.5707963267948966 nil]})

(defn ratio
  "Renders the trig ratios for angle1 and angle3 for the
   currently selected trig function."
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices :as triangle}]
  (let [{[numer1 denom1] :angle1
         [numer3 denom3] :angle3}
        ((keyword (str/replace @trig-fn "\\" "")) (trig-ratios triangle))]
    [:div (tex (str @trig-fn "(\\angle{" label1 "})=\\dfrac{" numer1 "}{" denom1 "}")) [:p]
     (tex (str @trig-fn "(\\angle{" label3 "})=\\dfrac{" numer3 "}{" denom3 "}"))]))

(defonce obtuse? (r/atom {:angle1 false :angle2 false :angle3 false}))

(defn angle-tex [f deg n1 d1 n2 d2]
  (tex (str f deg
            "\\degree)=\\dfrac{"
            n1 "}{" d1
            "}=\\dfrac{"
            n2 "}{" d2 "}")))

(defn angle-sin []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[line1 line2 line3] :lines
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
                     (or line2 (str label3 label2))
                     (= @selected-angle 3)
                     (str label2 label1)
                     :else line1)
               (or line3 (str label1 label3)))))

(defn angle-cos []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[line1 line2 line3] :lines
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
                     (or line1 (str label1 label2))
                     (= @selected-angle 3)
                     line2
                     :else (str label3 label2))
               (or line3 (str label1 label3)))))

(defn angle-tan []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[line1 line2 line3] :lines
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
                     (or line2 (str label3 label2))
                     (= @selected-angle 3)
                     (str label2 label1)
                     :else line1)
               (cond (= @selected-angle 1)
                     (or line1 (str label1 label2))
                     (= @selected-angle 3) line2
                     :else (str label3 label2)))))

(defn angle-data []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])]
  [:div [:h3 "Angle "
         (tex (str (get-in @tri [:vertices (dec @selected-angle)]) "="
                   degrees "\\degree"))]
   [:p] [angle-sin]
   [:p] [angle-cos]
   [:p] [angle-tan]]))

(defn line-data []
  (let [degrees (get-in @tri [:angles (dec @selected-angle)])
        {[line1 line2 line3] :lines
         [angle1 angle2 angle3] :angles
         [label1 label2 label3] :vertices} @tri]
    [:div [:h3 "Line "
           (let [p1 (get-in @tri [:vertices (dec @selected-line)])
                 p2 (get-in @tri [:vertices (mod @selected-line 3)])]
             (tex (str p2 p1)))]
     (cond
       (= @selected-line 1)
       [:div
        (tex (str line3 "\\cdot\\sin(" angle3 "\\degree)")) [:p]
        (tex (str line2 "\\cdot\\tan(" angle3 "\\degree)")) [:p]
        (tex (str line3 "\\cdot\\cos(" angle1 "\\degree)")) [:p]
        (tex (str "\\dfrac{" line2 "}{\\tan(" angle1 "\\degree)}"))]
       (= @selected-line 2) nil
       (= @selected-line 3)
       [:div
        (tex (str "\\dfrac{" line2 "}{\\sin(" angle1 "\\degree)}")) [:p]
        (tex (str "\\dfrac{" line1 "}{\\cos(" angle1 "\\degree)}")) [:p]
        (tex (str "\\dfrac{" line1 "}{\\sin(" angle3 "\\degree)}")) [:p]
        (tex (str "\\dfrac{" line2 "}{\\cos(" angle3 "\\degree)}"))])]))

(defn pi-frac [n] 
  (cond 
    (= n pi) (tex "\\pi")
    (int? (/ pi n))
    (tex (str "\\dfrac{\\pi}{" (/ pi n) "}"))
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
                            (get (:lines triangle) (dec side))))]
    (and (pos? length)
         (= (get (:lines triangle) (mod (first common-angles) 3))
            (get (:lines triangle) (mod (last common-angles) 3))))))

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
                            (get (:lines triangle) (dec side))))]
    (-> triangle
        (assoc-in [:lines (mod (first common-angles) 3)] length)
        (assoc-in [:lines (mod (last common-angles) 3)] length))))

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
                            (get (:lines @tri) (dec side))))]
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
  "Calculates the 3rd angle of a right triangle
   using the Pythagorean Theorem.
   
   TODO: Only implemented for a single config,
   where the right angle is the 2nd one."
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    :as triangle}]
  (let [right-angle
        (first (filter #(= (get (:angles triangle) %) (/ pi 2)) 
                       (range 2)))]
    (cond (= right-angle 1)
          (assoc-in triangle [:lines 2] 
                    (sqrt (+ (sq line1) (sq line2)))))))

(defn pythagoras?
  "Returns true if the triangle has 2 sides defined
   but not the hypotenuse, and thus could be solved
   using the Pythagorean Theorem.

   TODO: Only implemented for a single config,
   where the right angle is the 2nd one."
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
    (let [right-angle
        (first (filter #(= (get (:angles triangle) %) (/ pi 2)) 
                       (range 2)))]
      (cond (= right-angle 1)
            (and (not (pos? line3)) (pos? line1) (pos? line2)))))

(defonce deg-rad (r/atom "rad"))

(defn tri-angles
  [{[angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div
   [:div (tex (str "\\angle{" label1 "}=")) " "
    (pi-frac (if (= @deg-rad "deg") (deg angle1) angle1))] [:p]
   [:div (tex (str "\\angle{" label2 "}=")) " "
    (pi-frac (if (= @deg-rad "deg") (deg angle2) angle2))] [:p]
   [:div (tex (str "\\angle{" label3 "}=")) " "
    (pi-frac (if (= @deg-rad "deg") (deg angle3) angle3))] [:p]])

(defn tri-sides
  [{[line1 line2 line3] :lines
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div
   [:div (tex (str "\\overline{" label1 label2 "}=")) " "
    (pi-frac line1)] [:p]
   [:div (tex (str "\\overline{" label2 label3 "}=")) " "
    (pi-frac line2)] [:p]
   [:div (tex (str "\\overline{" label3 label1 "}=")) " "
    (tex (latex/sqrt-tex (pi-frac line3)))] [:p]])

(defn tri-data
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div
   [:div
    [button "Rad" #(reset! deg-rad "rad")]
    [button "Deg" #(reset! deg-rad "deg")] [:p]]
   [tri-angles triangle]
   [tri-sides triangle]
   (when (right? triangle)
     [:div
      [:p (str "This is a right triangle.")]])
   (when (and (isosceles? triangle)
              (not (iso-sides? triangle)))
     [iso triangle])
   (when (= 2 (count (filter pos? [angle1 angle2 angle3])))
     [:div
      [:div "The interior angle measures of a triangle always add up to " (tex "\\pi") "."]
      [button
       [:span "Compute "
        (tex (str "\\angle{"
                  (cond (nil? angle1) label1
                        (nil? angle2) label2
                        (nil? angle3) label3) "}"))]
       #(do (swap! tri infer-angle-rad)
            (update-editor! (str @tri)))]])
   (when (pythagoras? triangle)
     [:div
      [:p "The hypotenuse can be calculated by the Pythagorean Theorem."]
      [button
       "Calculate hypotenuse"
       #(do (swap! tri hypotenuse)
            (update-editor! (str @tri)))]])])

(defn ratio-buttons []
  (into [:div]
   (for [function ["\\sin" "\\csc" "\\cos" "\\sec" "\\tan" "\\cot"]]
     [button (tex function) #(reset! trig-fn function)])))

(defn app []
    [:div#app
     [editor/editor (str @tri) !tri {:eval? true}]
     [:button
      {:on-click
       #(reset! tri (eval-all
                     (str "(def pi js/Math.PI)
                           (defn rad [deg] (* deg (/ pi 180)))"
                          (some-> @!tri .-state .-doc str))))}
      "Eval"] [:p]
     [tri-data @tri] [:p]
     [ratio-buttons] [:p]
     [ratio @tri] [:p]
     [button "Solve" #(do (swap! tri solve-triangle)
                          (update-editor! (str @tri)))]

     [render-triangle @tri]
     [uc/uc-1]
     [:div
      [angle-data]
      [line-data]]
      ;[los/law-of-sines "A" @tri]
     ])