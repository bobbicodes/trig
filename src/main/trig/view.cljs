(ns trig.view
  (:require [trig.pythagoras :as pythagoras]
            [trig.triangle :as tri :refer [triangle]]))

(defn app []
    ;[tri/app @triangle]
     [pythagoras/pythagorean-identity-sin "\\theta_1" "\\text{I}" 3 8])