(ns trig.view
  (:require [trig.pythagoras :as pythagoras]
            [trig.law-of-sines :as los]
            [trig.triangle :as tri :refer [triangle]]))

(defn app []
  [:div
   [tri/app @triangle]
     ;[pythagoras/pythagorean-identity-sin "\\theta_1" "\\text{I}" 3 8]
   ]
  )