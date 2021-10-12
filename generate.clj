(ns uc.generate
  (:require [clojure.java.shell :refer [sh]]))

(def tex2svg "/usr/local/lib/nodejs/node-v14.17.0-linux-arm64/lib/node_modules/mathjax-node-cli/bin/tex2svg")

(def small-nums
  (for [n (range 10)]
    {:n (str "small-" n)
     :f (str "\\small{" n "}")}))

(def small-neg-nums
  (for [n (range 10)]
    {:n (str "small-neg" n)
     :f (str "\\small{\\llap{-}" n "}")}))

small-nums

(def formulas [{:n "1"
                :f "\small{1}"}
               {:n "-1"
                :f "\small{\llap{-}1}"}
               {:n "tau-coord"
                :f "\\left[1\\quad0\\right]"}
               {:n "tau_2"
                :f "\\frac{\\tau}{2}"}
               {:n "etau_2"
                :f "e^{i\\tau/2}"}
               {:n "tau_2-coord"
                :f "\\left[-1\\quad0\\right]"}
               {:n "tau_3"
                :f "\\frac{\\tau}{3}"}
               {:n "etau_3"
                :f "e^{i\\tau/3}"}
               {:n "tau_3-coord"
                :f "\\left[-\\frac{1}{2}\\quad\\frac{\\sqrt{3}}{2}\\right]"}
               {:n "tau_4"
                :f "\\frac{\\tau}{4}"}
               {:n "etau_4"
                :f "e^{i\\tau/4}"}
               {:n "tau_4-coord"
                :f "\\left[0\\quad1\\right]"}
               {:n "tau_6"
                :f "\\frac{\\tau}{6}"}
               {:n "etau_6"
                :f "e^{i\\tau/6}"}
               {:n "tau_6-coord"
                :f "\\left[\\frac{1}{2}\\quad\\frac{\\sqrt{3}}{2}\\right]"}
               {:n "tau_12"
                :f "\\frac{\\tau}{12}"}
               {:n "etau_12"
                :f "e^{i\\tau/12}"}
               {:n "tau_12-coord"
                :f "\\left[\\frac{\\sqrt{3}}{2}\\quad\\frac{1}{2}\\right]"}
               {:n "tau_8"
                :f "\\frac{\\tau}{8}"}
               {:n "etau_8"
                :f "e^{i\\tau/8}"}
               {:n "tau_8-coord"
                :f "\\left[\\frac{\\sqrt{2}}{2}\\quad\\frac{\\sqrt{2}}{2}\\right]"}
               {:n "3tau_4"
                :f "\\frac{3\\tau}{4}"}
               {:n "e3tau_4"
                :f "e^{i3\\tau/4}"}
               {:n "3tau_4-coord"
                :f "\\left[0\\quad-1\\right]"}
               {:n "key1"
                :f "e^{i\\theta} = cos{\\theta} + isin{\\theta}"}
               {:n "key2"
                :f "\\mapsto\\ \\left[\\cos{\\theta}\\quad\\sin{\\theta}\\right]"}])

(defn renderer [{:keys [n f]}]
  (spit (str "public/img/" n ".svg") (:out (sh tex2svg (str "\\begin{equation} " f "\\end{equation}")))))

(defn -main [& args]
  (doall (map renderer small-neg-nums)))

(-main)
