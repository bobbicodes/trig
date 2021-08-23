(ns trig.pythagoras
  (:require ["katex" :as katex]))

(defn tex [text]
  [:span {:ref (fn [el]
                 (when el
                   (try
                     (katex/render text el (clj->js
                                            {:throwOnError false}))
                     (catch :default e
                       (js/console.warn "Unexpected KaTeX error" e)
                       (aset el "innerHTML" text)))))}])

(defn text [s]
  [:span s])

 (defn pythagorean-identity []
   [:div
    (text "The angle ") (tex "\\theta_1")
    (text " is located in Quadrant ") (tex "\\text{II}") (text ", and ")
    (tex "\\sin(\\theta_1)=\\dfrac{9}{41}")  (text ".")
    [:p]
    (text "What is the value of ") (tex "\\cos(\\theta_1)") (text "?")
    [:p]
    (tex "\\begin{aligned}\\cos^2(\\theta_1)+\\sin^2(\\theta_1)&=1 \\\\\\\\\\cos^2(\\theta_1)&={1-\\sin^2(\\theta_1)} \\\\&=1-\\left(\\dfrac{9}{41}\\right)^2 \\\\&=\\dfrac{1600}{1681}\\end{aligned}​​")
    [:p]
    (tex "\\begin{aligned}\\cos(\\theta_1)&=-\\sqrt{\\cos^2(\\theta_1)} \\\\&=-\\sqrt{\\dfrac{1600}{1681}} \\\\&=-\\dfrac{40}{41}\\end{aligned}​")])