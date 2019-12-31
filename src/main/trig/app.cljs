(ns trig.app
  (:require [reagent.core :as r]
            [trig.latex :as latex]))

(defonce triangle-atom 
  (r/atom {:base   12
           :height 5
           :hypotenuse nil
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

(defn button [label onclick]
  [:button
   {:on-click onclick}
   label])

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
    [:svg {:width     "100%"
           :view-box  (str "0 0 " (+ 2 base) " " (+ 2 height))}
     [polygon 1 1 1 (inc height) (inc base) (inc height)]
     [latex/render-letter (keyword angle1) 0 0]
     [latex/render-letter (keyword angle2) (inc base) (inc height)]
     [latex/render-letter (keyword angle3) 0 (inc height)]
     [latex/render-num height -350 (+ 300 (* 40 height))]
     [latex/render-num base (* base 18) (+ 750 (* height 30))]
     [latex/render-num (.sqrt js/Math (+ (* height height) (* base base))) -200 (+ 200 (* 18 height))]
     [right-angle-box 1 height]]))

(defn sin-deg
  "accepts a value in degrees, converts it to radians and returns the sine"
  [deg]
  (.sin js/Math (* deg (/ js/Math.PI 180))))

(defn asin-deg
  "accepts a value in degrees, converts it to radians and returns the arcsine"
  [deg]
  (* (.asin js/Math deg) (/ 180 js/Math.PI)))

(defn acos-deg
  "accepts a value in degrees, converts it to radians and returns the arcosine"
  [deg]
  (* (.acos js/Math deg) (/ 180 js/Math.PI)))

(defn tan-deg 
  "accepts a value in degrees, converts it to radians and returns the tangent"
  [deg] 
  (.tan js/Math (* deg (/ js/Math.PI 180))))

(defn atan-deg
  "accepts a value in degrees, converts it to radians and returns the arctangent"
  [deg]
  (* (.atan js/Math deg) (/ 180 js/Math.PI)))

(defn cos-deg
  "accepts a value in degrees, converts it to radians and returns the cosine"
  [deg]
  (.cos js/Math (* deg (/ js/Math.PI 180))))

(defn solve-triangle [triangle]
  (let [{:keys [base height hypotenuse angle1 angle2]} triangle]
    (cond
      (and
       (pos? (:degrees angle1))
       (pos? hypotenuse)) (assoc triangle
                                 :height (* hypotenuse (cos-deg (:degrees angle1)))
                                 :base (* hypotenuse (sin-deg (:degrees angle1))))
      (and
       (pos? (:degrees angle1))
       (pos? height)) (assoc triangle
                             :base (* height (tan-deg (:degrees angle1)))
                             :hypotenuse (/ height (cos-deg (:degrees angle1))))
      (and
       (pos? (:degrees angle1))
       (pos? base)) (assoc triangle
                           :height (/ base (tan-deg (:degrees angle1)))
                           :hypotenuse (/ base (sin-deg (:degrees angle1))))
      (and
       (pos? (:degrees angle2))
       (pos? hypotenuse)) (assoc triangle
                                 :base (* hypotenuse (cos-deg (:degrees angle2)))
                                 :height (* hypotenuse (sin-deg (:degrees angle2))))
      (and
       (pos? (:degrees angle2))
       (pos? height)) (assoc triangle
                             :hypotenuse (/ height (sin-deg (:degrees angle2)))
                             :base (/ height (tan-deg (:degrees angle2))))
      (and
       (pos? (:degrees angle2))
       (pos? base)) (assoc triangle
                           :height (* base (tan-deg (:degrees angle2)))
                           :hypotenuse (/ base (cos-deg (:degrees angle2))))
      ;; solve for angles
      (and (pos? base) (pos? hypotenuse)) (assoc-in (assoc-in @triangle-atom [:angle1 :degrees] (asin-deg (/ base hypotenuse)))
                                                    [:angle2 :degrees] (acos-deg (/ base hypotenuse)))
      (and (pos? height) (pos? hypotenuse)) (assoc-in (assoc-in @triangle-atom [:angle1 :degrees] (acos-deg (/ height hypotenuse)))
                                                      [:angle2 :degrees] (asin-deg (/ height hypotenuse)))
      (and (pos? height) (pos? base))  (assoc-in (assoc-in @triangle-atom [:angle1 :degrees] (atan-deg (/ base height)))
                                                 [:angle2 :degrees] (atan-deg (/ height base)))
      :else "Does not compute")))

(comment 
  (.asin js/Math (* (/ js/Math.PI 180) (/ 4 6)))

  
  (number? (:hypotenuse @triangle-atom))
  (/ 3 (sin-deg 20))
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
      [input "number" " Hypotenuse: " (:hypotenuse @triangle-atom) #(swap! triangle-atom assoc :hypotenuse (-> % .-target .-value js/parseInt))]
      [:div
       [input "number" (str "∠" angle1 ": ") (:degrees (:angle1 @triangle-atom)) #(swap! triangle-atom assoc-in [:angle1 :degrees] (-> % .-target .-value js/parseInt))]
       [input "number" (str "∠" angle2 ": ") (:degrees (:angle2 @triangle-atom)) #(swap! triangle-atom assoc-in [:angle2 :degrees] (-> % .-target .-value js/parseInt))]]]
     [button "Solve" #(swap! triangle-atom solve-triangle)]
[button "Clear" (fn [e] (swap! triangle-atom #(assoc % :base nil :height nil :hypotenuse nil))
                  (swap! triangle-atom assoc-in [:angle1 :degrees] nil)
                  (swap! triangle-atom assoc-in [:angle2 :degrees] nil))]
     [right-triangle base height]
     [:p (str "sin(∠" angle1 "): " base " / " hypotenuse)]
     [:p (str "cos(∠" angle1 "): " height " / " hypotenuse)]
     [:p (str "tan(∠" angle1 "): " base " / " height)]
     [:p (str "sin(∠" angle2 "): " height " / " hypotenuse)]
     [:p (str "cos(∠" angle2 "): " base " / " hypotenuse)]
     [:p (str "tan(∠" angle2 "): " height " / " base)]
     [:p (str "Hypotenuse: " hypotenuse)]
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
