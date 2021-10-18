(ns trig.triangle
  (:require [reagent.core :as r]
            [trig.latex :as latex]
            ["katex" :as katex]
            [trig.editor :as editor]
            [sci.core :as sci]
            [trig.law-of-sines :as los]
            [trig.uc :as uc]
            [trig.sin :as sin]
            [cljs.spec.alpha :as s]))

(defonce tri
  (r/atom {:vertices ["A" "B" "C"]
           :lines [9 11 15]
           :angles [0 81 0]}))

(def pi js/Math.PI)

(defn sin [deg]
  (.sin js/Math (* deg (/ js/Math.PI 180))))

(defn cos [deg]
  (.cos js/Math (* deg (/ js/Math.PI 180))))

(defn tan [deg]
  (.tan js/Math (* deg (/ js/Math.PI 180))))

(defn asin [deg]
  (* (.asin js/Math deg) (/ 180 js/Math.PI)))

(defn acos [deg]
  (* (.acos js/Math deg) (/ 180 js/Math.PI)))

(defn atan [deg]
  (* (.atan js/Math deg) (/ 180 js/Math.PI)))

(defn csc
  "Returns the cosecant, the reciprocal of the sine,
   the ratio of the hypotenuse to the side opposite 
   a given angle in a right triangle."
  [h o]
  (/ h o))

(defn sec
  "Returns the secant, the reciprocal of the cosine,
   the ratio of the hypotenuse to the side adjacent to 
   a given angle in a right triangle."
  [h a]
  (/ h a))

(defn cot
  "Returns the cotangent, the reciprocal of the tangent,
   the ratio of the adjacent side 
   to the opposite side of a right triangle."
  [a o]
  (/ a o))

(defn solve-sides [{[line1 line2 line3] :lines
                    [angle1 angle2 angle3] :angles
                    [label1 label2 label3] :vertices
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

(infer-angle @tri)

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

(defn law-of-sines [{[line1 line2 line3] :lines
                     [angle1 angle2 angle3] :angles
                     [label1 label2 label3] :vertices
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
      (assoc triangle :angles [(asin (/ (* line2 (sin angle3))
                                        line1)) angle2 angle3])
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
                       [label1 label2 label3] :vertices
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
  "If 2 angles defined, will calculate the 3rd."
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
    (if (= 2 (count (filter pos? [angle1 angle2 angle3])))
       (infer-angle triangle)
      (-> triangle
          law-of-sines
          law-of-cosines)))

(defn input [type label value on-change]
  [:label label
   [:input
    {:style     {:width 40}
     :type      type
     :value     value
     :on-change on-change}]])

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])

(defn polygon [& points]
  (let [{[line1 line2 line3] :lines} @tri
        max-side (max line1 line2 line3)]
    [:polygon
     {:stroke       "#61e2ff"
      :stroke-width (/ max-side 100)
      :fill         "none"
      :points       (apply str (interpose " " points))}]))

(defn right-angle-box [x-pos y-pos w]
  [:rect
   {:width        w
    :height       w
    :fill         "none"
    :x            x-pos
    :y            y-pos
    :stroke       "#61e2ff"
    :stroke-width 0.05}])

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(defn sq [n]
  (* n n))

(defn dist [x1 y1 x2 y2]
  (.sqrt js/Math (+ (sq (- x2 x1))
                    (sq (- y2 y1)))))

(defonce selected-line (r/atom nil))
(defonce selected-angle (r/atom nil))

;; see https://math.stackexchange.com/questions/543961/
;; determine-third-point-of-triangle-when-two-points-and-all-sides-are-known
(defn render-triangle [{[line1 line2 line3] :lines
                        [angle1 angle2 angle3] :angles
                        [label1 label2 label3] :vertices}]
  (let [rad1 (* angle1 (/ js/Math.PI 180))
        rad2 (* angle2 (/ js/Math.PI 180))
        place-line1 [0 0 0 line1]
        cy (/ (- (+ (sq line1) (sq line3)) (sq line2))
              (* 2 line1))
        cx (.sqrt js/Math (- (sq line3) (sq cy)))
        height (inc (max (inc line1) cy (inc (.abs js/Math (- cy line1)))))
        max-side (max line1 line2 line3)
        hovered (r/atom nil)]
    (fn []
      (let [{[line1 line2 line3] :lines
             [angle1 angle2 angle3] :angles
             [label1 label2 label3] :vertices} @tri]
        [:div [:svg {:width    "100%"
                     :view-box
                     (str (- (/ max-side 15)) " "
                          (if (neg? cy)
                            (+ cy (- (/ max-side 20)))
                            (- (/ max-side 18))) " "
                          (+ 3 max-side) " "
                          (- (* 1.1 height) 1))}
               [:g
                (when (= 90 angle2)
                  [right-angle-box 0 (- line1 0.45) (/ height 15)])
                [:line {:x1 0 :x2 0 :y1 0 :y2 line1 :stroke-linecap "round"
                        :stroke (if (or (= @selected-line 1)
                                        (= @hovered 1)) "magenta" "#61e2ff")
                        :stroke-width (if (= @hovered 1) (/ max-side 75) (/ max-side 75))
                        :on-mouse-over #(reset! hovered 1) :on-mouse-out #(reset! hovered nil)
                        :on-click #(reset! selected-line 1)}]
                [:line {:x1 0 :x2 cx :y1 line1 :y2 cy :stroke-linecap "round"
                        :stroke (if (or (= @selected-line 2)
                                        (= @hovered 2)) "magenta" "#61e2ff")
                        :stroke-width (if (= @hovered 2) (/ max-side 75) (/ max-side 75))
                        :on-mouse-over #(reset! hovered 2) :on-mouse-out #(reset! hovered nil)
                        :on-click #(reset! selected-line 2)}]
                [:line {:x1 cx :x2 0 :y1 cy :y2 0 :stroke-linecap "round"
                        :stroke (if (or (= @selected-line 3)
                                        (= @hovered 3)) "magenta" "#61e2ff")
                        :stroke-width (if (= @hovered 3) (/ max-side 75) (/ max-side 75))
                        :on-mouse-over #(reset! hovered 3) :on-mouse-out #(reset! hovered nil)
                        :on-click #(reset! selected-line 3)}]]
               [:g
                [latex/render-letter
                 (keyword label1) (- (/ height 16)) (+ (- (/ max-side 20)) 0.4) (/ height 16000)
                 #(reset! selected-angle 1) (if (or (= @hovered "Angle 1")
                                                    (= @selected-angle 1)) "cyan" "#ffcc00")]
                [latex/render-letter
                 (keyword label2) (+ (- (/ max-side 20)) -0.1) line1 (/ height 16000)
                 #(reset! selected-angle 2) (if (or (= @hovered "Angle 2")
                                                    (= @selected-angle 2)) "cyan" "#ffcc00")]
                [latex/render-letter
                 (keyword label3) (+ (/ max-side 40) cx) (+ (- cy (/ max-side 45)) 0.2) (/ height 16000)
                 #(reset! selected-angle 3) (if (or (= @hovered "Angle 3")
                                                    (= @selected-angle 3)) "cyan" "#ffcc00")]]
               [:g ; enlarged click targets for selecting angles
                [:circle {:cx (- (/ max-side 20)) :cy (+ (- (/ max-side 20)) 0.4) :r (/ max-side 15)
                          :on-mouse-over #(reset! hovered "Angle 1") 
                          :on-mouse-out #(reset! hovered nil)
                          :visibility "hidden" 
                          :on-click #(reset! selected-angle 1) :pointer-events "all"}]
                [:circle {:cx (+ (- (/ max-side 20)) -0.1) :cy line1 :r (/ max-side 15)
                          :on-mouse-over #(reset! hovered "Angle 2") 
                          :on-mouse-out #(reset! hovered nil)
                          :visibility "hidden" 
                          :on-click #(reset! selected-angle 2) :pointer-events "all"}]
                [:circle {:cx (+ (/ max-side 40) cx) :cy (+ (- cy (/ max-side 45)) 0.2) :r (/ max-side 15)
                          :on-mouse-over #(reset! hovered "Angle 3") 
                          :on-mouse-out #(reset! hovered nil)
                          :visibility "hidden" 
                          :on-click #(reset! selected-angle 3) :pointer-events "all"}]]]]))))

(defn ratios [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} triangle]
    [:div
     [:p (str "sin(∠" angle1 "): " line1 " / " line3)]
     [:p (str "csc(∠" angle1 "): " line3 " / " line1)]
     [:p (str "cos(∠" angle1 "): " line2 " / " line3)]
     [:p (str "sec(∠" angle1 "): " line3 " / " line2)]
     [:p (str "tan(∠" angle1 "): " line1 " / " line2)]
     [:p (str "cot(∠" angle1 "): " line2 " / " line1)]
     [:p (str "sin(∠" angle2 "): " line2 " / " line3)]
     [:p (str "csc(∠" angle2 "): " line3 " / " line2)]
     [:p (str "cos(∠" angle2 "): " line1 " / " line3)]
     [:p (str "sec(∠" angle2 "): " line3 " / " line1)]
     [:p (str "tan(∠" angle2 "): " line2 " / " line1)]
     [:p (str "cot(∠" angle2 "): " line1 " / " line2)]
     [:p (str "line3: " line3)]]))

(defonce obtuse? (r/atom {:angle1 false :angle2 false :angle3 false}))

(defn tex [text]
  [:span 
   {:ref
    (fn [el]
      (when el
        (try
          (katex/render text el (clj->js {:throwOnError false}))
          (catch :default e
            (js/console.warn "Unexpected KaTeX error" e)
            (aset el "innerHTML" text)))))}])

(defonce trig-fn (r/atom "\\sin"))

(defonce !tri (r/atom @tri))

(defn eval-all [s]
  (try (sci/eval-string s {:classes {'js goog/global :allow :all}})
       (catch :default e
         (str e))))

(defn update-editor! [text]
  (let [end (count (some-> @!tri .-state .-doc str))]
    (.dispatch @!tri #js{:changes #js{:from 0 :to end :insert text}})))

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

(defn infer-angle-rad
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  (cond
    (nil? angle1)
    (assoc triangle :angles [(- pi angle2 angle3)
                             angle2 angle3])
    (nil? angle2)
    (assoc triangle :angles [angle1 (- pi angle1 angle3) angle3])
    (nil? angle3)
    (assoc triangle :angles [angle1 angle2 (- pi angle2 angle1)])))

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

{:vertices ["R" "T" "S"]
 :lines [3 nil nil]
 :angles [nil (/ pi 2) (/ pi 4)]}
(reset! selected-line 1)
(reset! selected-angle 1)

(defn pi-frac [n]
  (cond 
    (= n pi) (tex "\\pi")
    (int? (/ pi n))
    (tex (str "\\dfrac{\\pi}{" (/ pi n) "}"))
    :else n))

(defn isosceles? [tri]
  (= #{1 2}
     (set (vals (frequencies (:angles tri))))))

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

(iso-sides? @tri)

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
    (assoc-in
     (assoc-in triangle
               [:lines (mod (first common-angles) 3)] length)
     [:lines (mod (last common-angles) 3)] length)))

(iso-sides @tri)

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
      [:span "Compute sides"
       ]
      #(do (swap! tri iso-sides)
           (update-editor! (str triangle)))]]))

(defn tri-data
  [{[line1 line2 line3] :lines
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices
    :as triangle}]
  [:div
   [:div (tex (str "\\angle{" label1 "}=")) " "
    (pi-frac angle1)] [:p]
   [:div (tex (str "\\angle{" label2 "}=")) " "
    (pi-frac angle2)] [:p]
   [:div (tex (str "\\angle{" label3 "}=")) " "
    (pi-frac angle3)] [:p]
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
            (update-editor! (str @tri)))]])])

(defn app []
    [:div#app
     [editor/editor (str @tri) !tri {:eval? true}]
     [:button
      {:on-click
       #(reset! tri (eval-all
                     (str "(def pi js/Math.PI)"
                          (some-> @!tri .-state .-doc str))))
       :style {:margin-top "1rem"}}
      "Eval"] [:p]
     [tri-data @tri] [:p]
     #_[button "Solve" #(do (swap! tri solve-triangle)
                            (update-editor! (str @tri)))]
     #_[:div
        [uc/uc]]
     [render-triangle @tri]
     [:div
      [angle-data]
      [line-data]]
      ;[los/law-of-sines "A" @tri]
     ])