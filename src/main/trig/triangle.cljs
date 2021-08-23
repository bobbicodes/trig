(ns trig.triangle
  (:require [reagent.core :as r]
            [trig.latex :as latex]))

(defonce triangle
  (r/atom {:line1   nil
           :line2 5
           :line3 nil
           :angle1 31
           :angle2 108
           :angle3 nil
           :label1 "A"
           :label2 "B"
           :label3 "C"}))

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

(defn solve-sides [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2]} triangle]
    (cond
      (and (pos? angle1) (pos? line3))
      (assoc triangle
             :line2 (* line3 (cos angle1))
             :line1 (* line3 (sin angle1)))
      (and (pos? angle1) (pos? line2))
      (assoc triangle
             :line1 (* line2 (tan angle1))
             :line3 (/ line2 (cos angle1)))
      (and (pos? angle1) (pos? line1))
      (assoc triangle
             :line2 (/ line1 (tan angle1))
             :line3 (/ line1 (sin angle1)))
      (and (pos? (:degrees angle2)) (pos? line3))
      (assoc triangle
             :line1 (* line3 (cos angle2))
             :line2 (* line3 (sin angle2)))
      (and (pos? (:degrees angle2)) (pos? line2))
      (assoc triangle
             :line3 (/ line2 (sin angle2))
             :line1 (/ line2 (tan angle2)))
      (and (pos? angle2) (pos? line1))
      (assoc triangle
             :line2 (* line1 (tan angle2))
             :line3 (/ line1 (cos angle2)))
      :else triangle)))

(defn infer-angle [triangle]
 (let [{:keys [angle1 angle2 angle3]} triangle] 
   (cond
     (nil? angle1)
     (assoc triangle :angle1 (- 180 angle2 angle3))
     (nil? angle2)
     (assoc triangle :angle2 (- 180 angle1 angle3))
     (nil? angle3)
     (assoc triangle :angle3 (- 180 angle2 angle1)))))


(defn solve-angles [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3]} triangle]
    (cond
      (= 2 (count (filter pos? [angle1 angle2 angle3])))
      (infer-angle triangle)
      (and (pos? line1) (pos? line3))
      (-> triangle
          (assoc :angle1 (asin (/ line1 line3))
                 :angle2 (acos (/ line1 line3))))
      (and (pos? line2) (pos? line3))
      (-> triangle
          (assoc :angle1 (acos (/ line2 line3))
                 :angle2 (asin (/ line2 line3))))
      (and (pos? line2) (pos? line1))
      (-> triangle
          (assoc :angle1 (atan (/ line1 line2))
                 :angle2  (atan (/ line2 line1))))
      :else triangle)))

(defn law-of-sines [triangle]
  (let [{:keys [angle1 angle2 angle3 line1 line2 line3]} triangle]
    (cond
      (and (pos? angle1) (pos? angle2) (pos? line2))
      (assoc triangle :line3 (* line2 (/ (sin angle2) (sin angle1))))
      (and (pos? angle1) (pos? angle2) (pos? line3))
      (assoc triangle :line2 (* line3 (/ (sin angle1) (sin angle2))))
      (and (pos? angle1) (pos? line1) (pos? line2))
      (assoc triangle :angle3 (asin (* (/ line1 line2) (sin angle1))))
      (and (pos? angle1) (pos? line2) (pos? line3))
      (assoc triangle :angle2 (asin (* (/ line3 line2) (sin angle1))))
      (and (pos? angle1) (pos? angle3) (pos? line1))
      (assoc triangle :line2 (* line1 (/ (sin angle1) (sin angle3))))
      (and (pos? angle1) (pos? angle3) (pos? line2))
      (assoc triangle :angle2 (* line2 (/ (sin angle3) (sin angle1))))
      (and (pos? angle1) (pos? angle3) (pos? line3))
      (assoc triangle :line2 (* line3 (/ (sin angle1) (sin angle3))))
      (and (pos? angle2) (pos? angle3) (pos? line1))
      (assoc triangle :line3 (* line1 (/ (sin angle2) (sin angle3))))
      (and (pos? angle2) (pos? angle3) (pos? line3))
      (assoc triangle :line1 (* line3 (/ (sin angle3) (sin angle2))))
      (and (pos? angle2) (pos? line1) (pos? line3))
      (assoc triangle :angle3 (asin (* (/ line1 line3) (sin angle2))))
      (and (pos? angle2) (pos? line2) (pos? line3))
      (assoc triangle :angle1 (asin (* (/ line2 line3) (sin angle2))))
      (and (pos? angle3) (pos? line1) (pos? line2))
      (assoc triangle :angle1 (asin (/ (* line2 (sin angle3))
                                       line1)))
      (and (pos? angle3) (pos? line1) (pos? line3))
      (assoc triangle :angle2 (asin (* (/ line3 line1) (sin angle3))))
      :else triangle)))

(comment
  (let [{:keys [angle1 angle2 angle3 line1 line2 line3]} @triangle]
    (cond
      (and (pos? angle1) (pos? angle2) (pos? line2))
      "poop")))

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

(defn law-of-cosines [triangle]
  (let [{:keys [angle1 angle2 angle3 line1 line2 line3]} triangle]
    (cond
      (and (pos? angle1) (pos? line1) (pos? line3))
      (assoc triangle :line2 (loc-side line3 line1 angle1))
      (and (pos? angle2) (pos? line1) (pos? line2))
      (assoc triangle :line3 (loc-side line1 line2 angle2))
      (and (pos? angle3) (pos? line2) (pos? line3))
      (assoc triangle :line1 (loc-side line2 line3 angle3))
      (and (pos? line1) (pos? line2) (pos? line3))
      (assoc triangle
             :angle1 (loc-angle line1 line2 line3)
             :angle2 (loc-angle line2 line3 line1)
             :angle3 (loc-angle line2 line1 line3))
      :else triangle)))

(comment
  (- 180 49)
  (let [{:keys [angle1 angle2 angle3]} @triangle]
    (= 2 (count (filter pos? [angle1 angle2 angle3]))))
  (law-of-cosines @triangle)
  )

(defn solve-triangle
  "If 2 angles defined, will calculate the 3rd."
  [triangle]
  (let [{:keys [angle1 angle2 angle3]} triangle]
    (if (= 2 (count (filter pos? [angle1 angle2 angle3])))
       (infer-angle triangle)
      (-> triangle
          law-of-sines
          law-of-cosines))))

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
  (let [{:keys [line1 line2 line3]} @triangle
        max-side (max line1 line2 line3)]
    [:polygon
     {:stroke       "#61e2ff"
      :stroke-width (/ max-side 100)
      :fill         "none"
      :points       (apply str (interpose " " points))}]))

(defn right-angle-box [x-pos y-pos]
  [:rect
   {:width        1
    :line2        1
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

(let [{:keys [line1 line2 line3]} @triangle]
  (/ (- (+ (sq line1) (sq line3)) (sq line2))
     (* 2 line1)))

;; see https://math.stackexchange.com/questions/543961/determine-third-point-of-triangle-when-two-points-and-all-sides-are-known
(defn render-triangle [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} triangle
        rad1 (* angle1 (/ js/Math.PI 180))
        rad2 (* angle2 (/ js/Math.PI 180))
        place-line1 [0 0 0 line1]
        cy (/ (- (+ (sq line1) (sq line3)) (sq line2))
              (* 2 line1))
        cx (.sqrt js/Math (- (sq line3) (sq cy)))
        max-side (max line1 line2 line3)]
    [:svg {:width    "100%"
           :view-box
           (str (- (/ max-side 15)) " "
                (if (neg? cy)
                  (+ cy (- (/ max-side 20)))
                  (- (/ max-side 18))) " "
                (+ 3 max-side) " "
                (* 1.4 max-side))}
     (apply polygon (conj place-line1 cx cy))
     [:g
      [latex/render-letter
       (keyword label1) (- (/ max-side 20)) (- (/ max-side 20)) (/ max-side 20000)]
      [latex/render-letter
       (keyword label2) (- (/ max-side 20)) line1 (/ max-side 20000)]
      [latex/render-letter
       (keyword label3) (+ (/ max-side 40) cx) (- cy (/ max-side 45)) (/ max-side 20000)]]
     #_[:g [latex/render-num line2 -350 (+ 300 (* 40 line2))]
        [latex/render-num line1 (* line1 18) (+ 750 (* line2 30))]
        [latex/render-num line3 -200 (+ 200 (* 18 line2))]]
     ;[right-angle-box 1 line2]
     ]))

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

(defn app []
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} @triangle]
    [:div#app
     ;[:h2 "Trigonometry with general triangles"]
     ;[:h3 "Solve for sides or angles"]
     [:div "Vertices: "
      [input "text" "" label1 #(swap! triangle assoc :label1 (-> % .-target .-value))] " "
      [input "text" "" label2 #(swap! triangle assoc :label2 (-> % .-target .-value))] " "
      [input "text" "" label3 #(swap! triangle assoc :label3 (-> % .-target .-value))] " "]
     [:div
      [input "number" (str label1 label2 ": ") (round line1 100) #(swap! triangle assoc :line1 (-> % .-target .-value js/parseFloat))] " "
      [input "number" (str label2 label3 ": ") (round line2 100) #(swap! triangle assoc :line2 (-> % .-target .-value js/parseFloat))] " "
      [input "number" (str label1 label3 ": ") (round line3 100) #(swap! triangle assoc :line3 (-> % .-target .-value js/parseFloat))] " "]
     [:div
      [input "number" (str "∠" label1 ": ") (round (if (:angle1 @obtuse?) (- 180 angle1) angle1) 1) #(swap! triangle assoc :angle1 (-> % .-target .-value js/parseFloat))] "° "
      [input "number" (str "∠" label2 ": ") (round (if (:angle2 @obtuse?) (- 180 angle2) angle2) 1) #(swap! triangle assoc :angle2 (-> % .-target .-value js/parseFloat))] "° "
      [input "number" (str "∠" label3 ": ") (round (if (:angle3 @obtuse?) (- 180 angle3) angle3) 1) #(swap! triangle assoc :angle3 (-> % .-target .-value js/parseFloat))] "° "]
     [:div
      [button "Solve" #(swap! triangle solve-triangle)]
      [button "Clear" #(swap! triangle assoc :line1 nil :line2 nil :line3 nil :angle1 nil :angle2 nil :angle3 nil)]]
     [render-triangle @triangle]]
    #_[pythagoras/pythagorean-identity-sin "\\theta_1" "\\text{I}" 3 8]))

(comment
  (pos? (:angle2 @triangle))
  (law-of-sines @triangle)
(solve-triangle @triangle))
