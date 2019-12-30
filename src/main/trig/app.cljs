(ns trig.app
  (:require [reagent.core :as r]
            [trig.latex :as latex]))

(defonce triangle-atom 
  (r/atom {:base   12
           :height 5
           :angle1 {:name "A"
                    :degrees nil}
           :angle2 {:name "B"
                    :degrees nil}
           :angle3 {:name "C"
                    :degrees 90}}))

(defn input [type label value on-change]
  [:label label
   [:input
    {:style     {:width            "7%"
                 :background-color "lightgray"}
     :type      type
     :value     value
     :on-change on-change}]])

(defn polygon [& points]
  [:polygon {:stroke "#000000"
             :stroke-width 0.1
             :fill "none"
             :points (apply str (interpose " " points))}])

(defn right-angle-box [x y]
  [:rect {:width        1
          :height       1
          :fill         "none"
          :x            x
          :y            y
          :stroke       "#000000"
          :stroke-width 0.05}])

(defn right-triangle [base height]
  (let [angle1 (:name (:angle1 @triangle-atom))
        angle2 (:name (:angle2 @triangle-atom))
        angle3 (:name (:angle3 @triangle-atom))]
    [:svg {:width    "100%"
           :view-box (str "0 0 " (+ 2 base) " " (+ 2 height))}
     [polygon 1 1 1 (inc height) (inc base) (inc height)]
     [latex/render-letter (keyword angle1) 0 0]
     [latex/render-letter (keyword angle2) (inc base) (inc height)]
     [latex/render-letter (keyword angle3) 0 (inc height)]
     [latex/render-num height -350 (+ 300 (* 40 height))]
     [latex/render-num base (* base 18) (+ 750 (* height 30))]
     [latex/render-num (.sqrt js/Math (+ (* height height) (* base base))) -200 (+ 200 (* 18 height))]
     [right-angle-box 1 height]]))

(defn tan-deg 
  "accepts a value in degrees, converts it to radians and returns the tangent"
  [deg] 
  (.tan js/Math (* deg  (/ 3.14159 180))))

(comment 
  (* (:base @triangle-atom) (tan-deg 65))
  (count (:left-paren latex/characters))
  (latex/render-letters [[(:left-paren latex/characters) (:angle latex/characters)]] 0 20)
  )

(defn app []
  (let [base   (:base @triangle-atom)
        height (:height @triangle-atom)
        hypotenuse (.sqrt js/Math (+ (* height height) (* base base)))
        angle1 (:name (:angle1 @triangle-atom))
        angle2 (:name (:angle2 @triangle-atom))
        angle3 (:name (:angle3 @triangle-atom))]
    [:div#app
     [:h2 "Trigonometry with right triangles"]
     [:div "Angles: "
      [input "text" "" angle1 #(swap! triangle-atom assoc-in [:angle1 :name] (-> % .-target .-value))]
      [input "text" "" angle2 #(swap! triangle-atom assoc-in [:angle2 :name] (-> % .-target .-value))]
      [input "text" "" angle3 #(swap! triangle-atom assoc-in [:angle3 :name] (-> % .-target .-value))]]
     [:div
      [input "number" "Base: " base #(swap! triangle-atom assoc :base (-> % .-target .-value js/parseInt))]
      [input "number" " Height: " height #(swap! triangle-atom assoc :height (-> % .-target .-value js/parseInt))]
      [:div
       [input "number" (str "∠" angle1 ": ") (:degrees (:angle1 @triangle-atom)) 
        #(swap! triangle-atom assoc-in
                    [:angle1 :degrees] (-> % .-target .-value js/parseInt)
             [:height]  (* base (tan-deg (:degrees (:angle2 @triangle-atom)))))]
       [input "number" (str "∠" angle2 ": ") (:degrees (:angle2 @triangle-atom)) 
        #(do (swap! triangle-atom assoc-in
                    [:angle2 :degrees] (-> % .-target .-value js/parseInt))
             (swap! triangle-atom assoc :height (* base (tan-deg (-> % .-target .-value js/parseInt)))))]]]
     [right-triangle base height]
     [:p (str "sin(∠" angle1 "): " base " / " hypotenuse)]
     [:p (str "cos(∠" angle1 "): " height " / " hypotenuse)]
     [:p (str "tan(∠" angle1 "): " base " / " height)]
     [:p (str "sin(∠" angle2 "): " height " / " hypotenuse)]
     [:p (str "cos(∠" angle2 "): " base " / " hypotenuse)]
     [:p (str "tan(∠" angle2 "): " height " / " base)]
     [:textarea
      {:rows      5
       :cols      40
       :value     (str @triangle-atom)
       :read-only true}]]))

(defn render []
  (r/render [app]
            (.getElementById js/document "root")))

(defn ^:dev/after-load start []
  (render)
  (js/console.log "start"))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
