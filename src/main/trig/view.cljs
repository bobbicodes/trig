(ns trig.view
  (:require [trig.pythagoras :as pythagoras]
            [trig.law-of-sines :as los]
            [trig.triangle :as tri :refer [triangle]]
            [trig.sin :as sin :refer [w]]))

#_(defonce scale-factor
  (r/atom "5"))

#_(defn scale-slider
  [min max step]
  [:div (str (cond
               (int? step)   "Integer"
               (float? step) "Float"
               :else         (str "Step is of type: " (type step)))
             " " min "-" max ": ")
   [:br]
   [:input {:type      "range"
            :min       min
            :max       max
            :step      step
            :value     @scale-factor
            :on-change #(reset! scale-factor (-> % .-target .-value))
            :class     ["min-w-full"]}]])

(defn app []
  [:div

   [sin/x-slider -10 10 0.01]
   [sin/y-slider -1 1 0.01]
   [sin/render-fn @w]
   [:p]
   [sin/calc-graph]
   [sin/max-point]
   [sin/mid-point]
   [sin/min-point]
     ;[pythagoras/pythagorean-identity-sin "\\theta_1" "\\text{I}" 3 8]
   ]
  )