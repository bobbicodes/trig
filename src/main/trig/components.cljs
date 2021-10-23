(ns trig.components
  (:require 
   [reagent.core :as r]
   [trig.latex :as latex]
   [trig.math :refer [sq]]))

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

(defn right-angle-box [x-pos y-pos w]
  [:rect
   {:width        w
    :height       w
    :fill         "none"
    :x            x-pos
    :y            y-pos
    :stroke       "#61e2ff"
    :stroke-width 0.05}])

(defonce selected-line (r/atom nil))
(defonce selected-angle (r/atom nil))

(def pi js/Math.PI)

;; see https://math.stackexchange.com/questions/543961/
;; determine-third-point-of-triangle-when-two-points-and-all-sides-are-known
(defn render-triangle [{[line1 line2 line3] :lines
                        [angle1 angle2 angle3] :angles
                        [label1 label2 label3] :vertices}
                       triangle]
  (let [rad1 (* angle1 (/ js/Math.PI 180))
        rad2 (* angle2 (/ js/Math.PI 180))
        place-line1 [0 0 0 line1]
        cy (/ (- (+ (sq line1) (sq line3)) (sq line2))
              (* 2 line1))
        cx (.sqrt js/Math (- (sq line3) (sq cy)))
        height (inc (max (inc line1) cy (inc (.abs js/Math (- cy line1)))))
        max-side (max line1 line2 line3)
        hovered (r/atom nil)]
      [:div [:svg {:width    "100%"
                   :view-box
                   (str (- (/ max-side 15)) " "
                        (if (neg? cy)
                          (+ cy (- (/ max-side 20)))
                          (- (/ max-side 18))) " "
                        (+ 3 max-side) " "
                        (- (* 1.1 height) 1))}
             [:g
              (when (= (/ pi 2) angle2)
                [right-angle-box 0 (- line1 (* height 0.065)) (/ height 16)])
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
             [:g ; enlarged click/hover targets for selecting angles
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
                        :on-click #(reset! selected-angle 3) :pointer-events "all"}]]]]))
