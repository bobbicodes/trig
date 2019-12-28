(ns shadow-reagent.app
  (:require [reagent.core :as r]))

(defn app []
  [:div#app
   [:h1 "shadow-cljs reagent template"]])

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
