(ns trig.app
  (:require [reagent.core :as r]
            [trig.view :as view]))

(defn render []
  (r/render [view/app]
            (.getElementById js/document "root")))

(defn ^:dev/after-load start []
  (render)
  (js/console.log "start"))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
