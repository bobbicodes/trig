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


(defn solve-angles [{[line1 line2 line3] :lines
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

;; see https://math.stackexchange.com/questions/543961/determine-third-point-of-triangle-when-two-points-and-all-sides-are-known
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
      [:div [:svg {:width    "100%"
                   :view-box
                   (str (- (/ max-side 15)) " "
                        (if (neg? cy)
                          (+ cy (- (/ max-side 20)))
                          (- (/ max-side 18))) " "
                        (+ 3 max-side) " "
                        (inc height))}
             [:g
              (when (= 90 angle2)
                [right-angle-box 0 (- line1 0.45) (/ max-side 20)])
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
               (keyword label1) (- (/ max-side 20)) (+ (- (/ max-side 20)) 0.4) (/ max-side 20000)]
              [latex/render-letter
               (keyword label2) (+ (- (/ max-side 20)) -0.1) line1 (/ max-side 20000)]
              [latex/render-letter
               (keyword label3) (+ (/ max-side 40) cx) (+ (- cy (/ max-side 45)) 0.2) (/ max-side 20000)]]
             ]])))

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

(defn app []
    [:div#app
     [editor/editor (str @tri) !tri {:eval? true}]
     [:button {:on-click #(reset! tri (eval-all
                                       (str "(def pi js/Math.PI)"
                                            (some-> @!tri .-state .-doc str))))
               :style {:margin-top "1rem"}}
      "Eval"]
     [button "Solve" #(do (swap! tri solve-triangle)
                          (reset! !tri @tri))]
     #_[:div
      [:button {:on-click #(reset! trig-fn "\\sin")
                :style {:margin-top "1rem"}}
       (tex "sin")]
      [:button {:on-click #(reset! trig-fn "\\cos")
                :style {:margin-top "1rem"}}
       (tex "cos")]]
     #_[:div
      [uc/uc]]
     [render-triangle @tri]
     [:span "Line "
              (let [p1 (get-in @tri [:vertices (dec @selected-line)])
                    p2 (get-in @tri [:vertices (mod @selected-line 3)])]
                (tex (str p1 p2)))]
     #_[los/law-of-sines "A" @tri]])
