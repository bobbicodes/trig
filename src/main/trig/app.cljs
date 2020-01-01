(ns trig.app
  (:require [reagent.core :as r]
            [trig.latex :as latex]))

(defonce triangle
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

(defn right-angle-box [x y]
  [:rect 
   {:width        1
    :height       1
    :fill         "none"
    :x            x
    :y            y
    :stroke       "#000000"
    :stroke-width 0.05}])

(defn right-triangle [base height]
    [:svg {:width    "80%"
           :view-box (str "0 0 " (+ 2 base) " " (+ 2 height))}
     [polygon 1 1 1 (inc height) (inc base) (inc height)]
     [right-angle-box 1 height]])

(defn triangle-labels [triangle]
  (let [{:keys [base height hypotenuse angle1 angle2 angle3]} triangle]
    [:svg {:width    "80%"
           :view-box (str "0 0 " (+ 2 base) " " (+ 2 height))}
     [:g [latex/render-letter (keyword (:name angle1)) 0 0]
      [latex/render-letter (keyword (:name angle2)) (inc base) (inc height)]
      [latex/render-letter (keyword (:name angle3)) 0 (inc height)]]
     [:g [latex/render-num height -350 (+ 300 (* 40 height))]
      [latex/render-num base (* base 18) (+ 750 (* height 30))]
      [latex/render-num hypotenuse -200 (+ 200 (* 18 height))]]]))

(comment
  (right-triangle)
  (triangle-labels @triangle))

(defn sin [deg]
  (.sin js/Math (* deg (/ js/Math.PI 180))))

(defn asin [deg]
  (* (.asin js/Math deg) (/ 180 js/Math.PI)))

(defn acos [deg]
  (* (.acos js/Math deg) (/ 180 js/Math.PI)))

(defn tan [deg] 
  (.tan js/Math (* deg (/ js/Math.PI 180))))

(defn atan [deg]
  (* (.atan js/Math deg) (/ 180 js/Math.PI)))

(defn cos [deg]
  (.cos js/Math (* deg (/ js/Math.PI 180))))

(defn solve-triangle [triangle]
  (let [{:keys [base height hypotenuse angle1 angle2]} triangle]
    (cond
      (and (pos? (:degrees angle1)) (pos? hypotenuse))
      (assoc triangle
             :height (* hypotenuse (cos (:degrees angle1)))
             :base (* hypotenuse (sin (:degrees angle1))))
      (and (pos? (:degrees angle1)) (pos? height))
      (assoc triangle
             :base (* height (tan (:degrees angle1)))
             :hypotenuse (/ height (cos (:degrees angle1))))
      (and (pos? (:degrees angle1)) (pos? base))
      (assoc triangle
             :height (/ base (tan (:degrees angle1)))
             :hypotenuse (/ base (sin (:degrees angle1))))
      (and (pos? (:degrees angle2)) (pos? hypotenuse))
      (assoc triangle
             :base (* hypotenuse (cos (:degrees angle2)))
             :height (* hypotenuse (sin (:degrees angle2))))
      (and (pos? (:degrees angle2)) (pos? height))
      (assoc triangle
             :hypotenuse (/ height (sin (:degrees angle2)))
             :base (/ height (tan (:degrees angle2))))
      (and (pos? (:degrees angle2)) (pos? base))
      (assoc triangle
             :height (* base (tan (:degrees angle2)))
             :hypotenuse (/ base (cos (:degrees angle2))))
      ;; solve for angles
      (and (pos? base) (pos? hypotenuse))
      (assoc-in (assoc-in triangle
                          [:angle1 :degrees]
                          (asin (/ base hypotenuse)))
                [:angle2 :degrees]
                (acos (/ base hypotenuse)))
      (and (pos? height) (pos? hypotenuse))
      (assoc-in (assoc-in triangle 
                          [:angle1 :degrees] 
                          (acos (/ height hypotenuse)))
                [:angle2 :degrees] 
                (asin (/ height hypotenuse)))
      (and (pos? height) (pos? base))  
      (assoc-in (assoc-in triangle
                          [:angle1 :degrees]
                          (atan (/ base height)))
                [:angle2 :degrees]
                (atan (/ height base)))
      :else "Does not compute")))

(comment
  (triangle-labels @triangle)
  (.asin js/Math (* (/ js/Math.PI 180) (/ 4 6)))
  (solve-triangle @triangle)

  (number? (:hypotenuse @triangle))
  (/ 3 (sin 20))
  (* (:base @triangle) (tan 65))
  (count (:left-paren latex/characters))
  (latex/render-letters [[(:left-paren latex/characters) (:angle latex/characters)]] 0 20)
(/ (.round js/Math (* 100 27.266)) 100)
  )

(defn round-hundredths [n]
  (/ (.round js/Math (* 100 n)) 100))

(defn app []
  (let [base   (:base @triangle)
        height (:height @triangle)
        hypotenuse (.sqrt js/Math (+ (* height height) (* base base)))
        angle1 (:name (:angle1 @triangle))
        angle2 (:name (:angle2 @triangle))
        angle3 (:name (:angle3 @triangle))]
    [:div#app
     [:h2 "Trigonometry with right triangles"]
     [:div "Angles: "
      [input "text" "" angle1 #(swap! triangle assoc-in [:angle1 :name] (-> % .-target .-value))]
      [input "text" "" angle2 #(swap! triangle assoc-in [:angle2 :name] (-> % .-target .-value))]
      [input "text" "" angle3 #(swap! triangle assoc-in [:angle3 :name] (-> % .-target .-value))]]
     [:div
      [input "number" "Base: " base #(swap! triangle assoc :base (-> % .-target .-value js/parseInt))]
      [input "number" " Height: " height #(swap! triangle assoc :height (-> % .-target .-value js/parseInt))]
      [input "number" " Hypotenuse: " (:hypotenuse @triangle) #(swap! triangle assoc :hypotenuse (-> % .-target .-value js/parseInt))]
      [:div
       [input "number" (str "∠" angle1 ": ") (round-hundredths (:degrees (:angle1 @triangle))) #(swap! triangle assoc-in [:angle1 :degrees] (-> % .-target .-value js/parseInt))]
       [input "number" (str "∠" angle2 ": ") (round-hundredths (:degrees (:angle2 @triangle))) #(swap! triangle assoc-in [:angle2 :degrees] (-> % .-target .-value js/parseInt))]]]
     [:div 
      [button "Solve" #(swap! triangle solve-triangle)]
      [button "Clear" (fn [e] (swap! triangle #(assoc % :base nil :height nil :hypotenuse nil))
                        (swap! triangle assoc-in [:angle1 :degrees] nil)
                        (swap! triangle assoc-in [:angle2 :degrees] nil))]]
     [right-triangle base height]
     [triangle-labels @triangle]
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
       :value     (str @triangle)
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
