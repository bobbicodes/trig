(ns trig.view
  (:require [trig.pythagoras :as pythagoras]
            [trig.law-of-sines :as los]
            [trig.triangle :as tri :refer [triangle]]
            [trig.sin :as sin :refer [points]]))

(defn app []
  [:div
   [sin/x-slider -10 10 0.01]
   [sin/y-slider -1 1 0.01]
   [sin/render-fn @points]
   [:p]
   [sin/calc-graph]
   [sin/points-input]
     ;[pythagoras/pythagorean-identity-sin "\\theta_1" "\\text{I}" 3 8]
   ]
  )