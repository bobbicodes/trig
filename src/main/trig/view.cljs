(ns trig.view
  (:require [reagent.core :as r]
            [trig.latex :as latex]
            [trig.triangle :as tri :refer [triangle]]
            [trig.edn :as edn]))

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
  [:polygon
   {:stroke       "#61e2ff"
    :stroke-width 0.3
    :fill         "none"
    :points       (apply str (interpose " " points))}])

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

;; see https://math.stackexchange.com/questions/543961/determine-third-point-of-triangle-when-two-points-and-all-sides-are-known
(defn render-triangle [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} triangle
        rad1 (* angle1 (/ js/Math.PI 180))
        rad2 (* angle2 (/ js/Math.PI 180))
        place-line1 [0 0 0 line1]
        cy (/ (- (+ (sq line1) (sq line3)) (sq line2))
              (* 2 line1))
        cx (.sqrt js/Math (- (sq line3) (sq cy)))]
    [:svg {:width    "80%"
           :view-box (str "-2 -2 20 20")}
     (apply polygon (conj place-line1 cx cy))
     [:g [latex/render-letter (keyword label1) -1 -1]
      [latex/render-letter (keyword label2) -1 line1]
      [latex/render-letter (keyword label3) (+ 0.4 cx) cy]]
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
     [:h2 "Trigonometry with general triangles"]
     [:h3 "Solve for sides or angles"]
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
      [input "checkbox" "Obtuse" (:angle1 @obtuse?) #(swap! obtuse? update :angle1 not)]
      [input "checkbox" "Obtuse" (:angle2 @obtuse?) #(swap! obtuse? update :angle2 not)]
      [input "checkbox" "Obtuse" (:angle3 @obtuse?) #(swap! obtuse? update :angle3 not)]]
     [:div
      [button "Solve" #(swap! triangle tri/solve-triangle)]
      [button "Clear" #(swap! triangle assoc :line1 nil :line2 nil :line3 nil :angle1 nil :angle2 nil :angle3 nil)]]
     [render-triangle @triangle]
     ;[ratios @triangle]
     ;(into [] (edn/html-edn @triangle))
     #_[:div.edn-block
      [:span
       [:span.opener "{"]
       [:span.contents
        [:span.keyval
         [:span ":label1 "]
         [:span
          [:span.contents label1]
          [:span.closer "\n"]]]
        [:span.seperator " "]
        [:span.keyval
         [:span ":label2 "]
         [:span
          [:span.contents label2]
          [:span.closer "\n"]]]
        [:span.keyval
         [:span " :label3 "]
         [:span
          [:span.contents label3]
          [:span.closer "\n"]]]
        [:span.keyval
         [:span " :line1 "]
         [:span line1]
         [:span.closer "\n"]]
        [:span.seperator " "]
        [:span.keyval
         [:span ":line2 "]
         [:span line2]
         [:span.closer "\n"]]
        [:span.keyval
         [:span " :line3 "]
         [:span line3]
         [:span.closer "\n"]]
        [:span.keyval
         [:span " :angle1 "]
         [:span angle1]
         [:span.closer "\n"]]
        [:span.keyval
         [:span " :angle2 "]
         [:span angle2]
         [:span.closer "\n"]]
        [:span.keyval
         [:span " :angle3 "]
         [:span angle3]]]]
      [:span.closer "}"]]]))

(comment
  (let [rad (* (:angle1 @triangle) (/ js/Math.PI 180))
        line (:line2 @triangle)]
    [(* line (tri/cos rad)) (* line (tri/sin rad))]
    ))