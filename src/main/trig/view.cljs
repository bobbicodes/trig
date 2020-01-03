(ns trig.view
  (:require [trig.latex :as latex]
            [trig.triangle :as tri :refer [triangle]]))

(defn input [type label value on-change]
  [:label label
   [:input
    {:style     {:width 50}
     :type      type
     :value     value
     :on-change on-change}]])

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])

(defn polygon [& points]
  [:polygon
   {:stroke       "#000000"
    :stroke-width 0.1
    :fill         "none"
    :points       (apply str (interpose " " points))}])

(defn right-angle-box [x-pos y-pos]
  [:rect
   {:width        1
    :line2        1
    :fill         "none"
    :x            x-pos
    :y            y-pos
    :stroke       "#000000"
    :stroke-width 0.05}])

(defn round
  "Rounds n to decimal precision x: (round 9.278 10) -> 9.3"
  [n x]
  (/ (.round js/Math (* x n)) x))

(defn render-triangle [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} triangle]
    [:svg {:width    "80%"
           :view-box (str "0 0 " (+ 2 line1) " " (+ 2 line2))}
     [polygon 1 1 1 (inc line2) (inc line1) (inc line2)]
     [:g [latex/render-letter (keyword label1) 0 0]
      [latex/render-letter (keyword label2) (inc line1) (inc line2)]
      [latex/render-letter (keyword label3) 0 (inc line2)]]
     [:g [latex/render-num line2 -350 (+ 300 (* 40 line2))]
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

(defn app []
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} @triangle]
    [:div#app
     [:h2 "Trigonometry with general triangles"]
     [:div "Angles: "
      [input "text" "" label1 #(swap! triangle assoc :label1 (-> % .-target .-value))]
      [input "text" "" label2 #(swap! triangle assoc :label2 (-> % .-target .-value))]
      [input "text" "" label3 #(swap! triangle assoc :label3 (-> % .-target .-value))]]
     [:div
      [input "number" (str label1 label2 ":") (round line1 10) #(swap! triangle assoc :line1 (-> % .-target .-value js/parseFloat))]
      [input "number" (str label2 label3 ":") (round line2 10) #(swap! triangle assoc :line2 (-> % .-target .-value js/parseFloat))]
      [input "number" (str label1 label3 ":") (round line3 10) #(swap! triangle assoc :line3 (-> % .-target .-value js/parseFloat))]]
      [:div
       [input "number" (str "∠" label1 ": ") (round angle1 100) #(swap! triangle assoc :angle1 (-> % .-target .-value js/parseFloat))]
       [input "number" (str "∠" label2 ": ") (round angle2 100) #(swap! triangle assoc :angle2 (-> % .-target .-value js/parseFloat))]
       [input "number" (str "∠" label3 ": ") (round angle3 100) #(swap! triangle assoc :angle3 (-> % .-target .-value js/parseFloat))]]
     [:div 
      [button "Solve" #(swap! triangle tri/solve-triangle)]
      [button "Clear" (fn [] (swap! triangle #(assoc % :line1 nil :line2 nil :line3 nil))
                        (swap! triangle assoc :angle1 nil)
                        (swap! triangle assoc :angle2 nil))]]
     [render-triangle @triangle]
     [ratios @triangle]
     [:textarea
      {:rows      5
       :cols      40
       :value     (str @triangle)
       :read-only true}]]))