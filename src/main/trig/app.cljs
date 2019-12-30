(ns trig.app
  (:require [reagent.core :as r]
            [trig.latex :as latex]))

(defonce triangle-atom 
  (r/atom {:base   12
           :height 5
           :angle1 {:name "A"}
           :angle2 {:name "B"}
           :angle3 {:name "C"}}))

(defn input [type label value on-change]
  [:label label
   [:input
    {:style     {:width            "4%"
                 :background-color "lightgray"}
     :type      type
     :value     value
     :on-change on-change}]])

(defn digits [n]
  (if (>= n 10)
    (conj (digits (quot n 10))
          (rem n 10))
    [n]))

(defn render-num [n x y]
  (let [digits (digits n)
        count  (count digits)]
    (into [:svg {:width    (* 9 count)
                 :view-box (str "0 0 " (* 500 count) " " 1000)}]
          (for [d (range count)]
            [:path {:transform (str "translate(" (+ x (* d 35)) "," y ") scale(0.07)")
                    :stroke    "#000000"
                    :d         ((keyword (str (get digits d))) latex/numbers)}]))))

(defn render-letter [k x y]
  [:path {:transform (str "translate(" x "," y ")" "scale(0.001)")
          :stroke    "#000000"
          :d         (k latex/letters)}])

(defn render-letters [[letters] x y]
  (let [count  (count letters)]
    (into [:svg {:width    (* 22 count)
                 :height (* 9 count)
                 :view-box (str "0 0 " (* 1000 count) " " 2000)}]
          (for [c (range count)]
            [:path {:transform (str "translate(" (+ x  (* c 1000)) "," y ") scale(2)")
                    :stroke    "#000000"
                    :d         (get latex/letters c)}]))))

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
     [render-letter (keyword angle1) 0 0]
     [render-letter (keyword angle2) (inc base) (inc height)]
     [render-letter (keyword angle3) 0 (inc height)]
     [render-num height -100 (+ 300 (* 40 height))]
     [render-num base (* base 30) (+ 600 (* height 30))]
     [render-num (.sqrt js/Math (+ (* height height) (* base base))) (* base 20) (+ 100 (* 18 height))]
     [right-angle-box 1 height]
     ]))

(defn tangent [angle x y]
   [render-letters
    [[(:t latex/letters) (:a latex/letters) (:n latex/letters)
      (:left-paren latex/characters) (:angle latex/characters) angle (:right-paren latex/characters)]]
    x y])


(comment 
  
  (digits 10)
  (count (:left-paren latex/characters))
  (render-letters [[(:left-paren latex/characters) (:angle latex/characters)]] 0 20)
  )

(defn app []
  (let [base   (:base @triangle-atom)
        height (:height @triangle-atom)
        hypotenuse (.sqrt js/Math (+ (* height height) (* base base)))]
    [:div#app
     [:h2 "Trigonometry with right triangles"]
     [:div
      [input "number" "Base: " base #(swap! triangle-atom assoc :base (-> % .-target .-value js/parseInt))]
      [input "number" " Height: " height #(swap! triangle-atom assoc :height (-> % .-target .-value js/parseInt))]
      [:div
       [input "text" "" (str (:name (:angle1 @triangle-atom))) #(swap! triangle-atom assoc-in [:angle1 :name] (-> % .-target .-value))]
       [input "text" "" (str (:name (:angle2 @triangle-atom))) #(swap! triangle-atom assoc-in [:angle2 :name] (-> % .-target .-value))]
       [input "text" "" (str (:name (:angle3 @triangle-atom))) #(swap! triangle-atom assoc-in [:angle3 :name] (-> % .-target .-value))]]
      #_[number-input " ∠A: " (:A @triangle-atom) #(swap! triangle-atom assoc :A (-> % .-target .-value js/parseInt))]]
     [right-triangle base height]
     [:p (str "sin(∠A): " base " / " hypotenuse)]
     [:p (str "cos(∠A): " height " / " hypotenuse)]
     [:p (str "tan(∠A): " base " / " height)]
     [:p (str "sin(∠B): " height " / " hypotenuse)]
     [:p (str "cos(∠B): " base " / " hypotenuse)]
     [:p (str "tan(∠B): " height " / " base)]
     [:p (str @triangle-atom)]]))

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
