(ns trig.view
  (:require [trig.pythagoras :as pythagoras]
            [trig.law-of-sines :as los]
            [trig.triangle :as tri]
            [trig.sin :as sin :refer [tex render-fn points scale x-scale y-scale]]))

(defn app []
  [:div
   [:div [sin/points-input]
    [:div.flex-container
     [:div.flex-item
    (tex (str "\\large{f(x)="
              (render-fn @points)
              "}"))]]
    [:p]
    [sin/calc-graph]
    [:div.flex-container
     [:div.flex-item
      [sin/x-slider -10 10 0.01]
      [sin/y-slider -1 1 0.01]]
     [:div.flex-item
      [:button {:on-click #(reset! scale :x)} (sin/tex "x")]
      [:button {:on-click #(reset! scale :pi)} (sin/tex "\\pi")]]]
    [:button {:on-click #(do (reset! x-scale 1)
                             (reset! y-scale 1))} "Reset"]
    [tri/app]]
     #_[pythagoras/pythagorean-identity-sin 
      "\\theta_1" "\\text{IV}" 3 5]])