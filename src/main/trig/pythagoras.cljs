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

(defn sq [n] (* n n))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn prime-factors
  ([n] (prime-factors 2 n))
  ([f n]
   (if (> n 1)
     (if (zero? (mod n f))
       (cons f (prime-factors f (/ n f)))
       (recur (inc f) n)))))

(defn perfect-squares [s]
  (loop [items (sort s) pairs []]
    (if (empty? items) pairs
        (if (= (first items) (second items))
          (recur (drop 2 items) (conj pairs (first items)))
          (recur (rest items) pairs)))))

(defn simplify-sqrt [sqrt]
  (let [sq (reduce * (perfect-squares (prime-factors sqrt)))]
    [sq (/ sqrt (* sq sq))]))

(.sqrt js/Math 1600)

(sq 9)
(sq 41)

(defn pythagorean-identity-cos [angle quad numer denom]
  (let [sin (str "\\sin(" angle ")="
                 (when (neg? numer) "-")
                 "\\dfrac{" (.abs js/Math numer) "}{" denom "}")
        calc1-numer (- (sq denom) (sq numer))
        calc1-denom (sq denom)]
    [:div
     (text "The angle ") (tex angle)
     (text " is located in Quadrant ") (tex quad) (text ", and ")
     (tex sin)  (text ".")
     [:p]
     (text "What is the value of ") (tex (str "\\cos(" angle ")")) (text "?")
     [:p]
     (tex (str "\\begin{aligned}\\cos^2(" angle ")+\\sin^2(" angle ")&=1 \\\\\\\\\\cos^2(" angle ")&={1-\\sin^2(" angle ")} \\\\&=1-\\left("
               (when (neg? numer) "-")
               "\\dfrac{" (.abs js/Math numer) "}{" denom "}\\right)^2 \\\\&=\\dfrac{" calc1-numer "}{" calc1-denom "}\\end{aligned}​​"))
     [:p]
     (tex (str "\\begin{aligned}\\cos(" angle ")&="
               (when (or (= quad "\\text{II}") (= quad "\\text{III}")) "-")
               "\\sqrt{\\cos^2(" angle ")} \\\\&="
               (when (or (= quad "\\text{II}") (= quad "\\text{III}")) "-")
               "\\sqrt{\\dfrac{" calc1-numer "}{" calc1-denom "}} \\\\&="
               (when (or (= quad "\\text{II}") (= quad "\\text{III}")) "-")
               "\\dfrac{" (.sqrt js/Math calc1-numer) "}{" (.sqrt js/Math calc1-denom) "}\\end{aligned}​"))]))

(defn pythagorean-identity-sin [angle quad numer denom]
  (let [cos (str "\\cos(" angle ")=" 
                 (when (neg? numer) "-")
                 "\\dfrac{" (.abs js/Math numer) "}{" denom "}")
        calc1-numer (- (sq denom) (sq numer))
        calc1-denom (sq denom)]
    [:div
     (text "The angle ") (tex angle)
     (text " is located in Quadrant ") (tex quad) (text ", and ")
     (tex cos)  (text ".")
     [:p]
     (text "What is the value of ") (tex (str "\\sin(" angle ")")) (text "?")
     [:p]
     (tex (str "\\begin{aligned}\\cos^2(" angle ")+\\sin^2(" angle ")&=1 \\\\\\\\\\sin^2(" angle ")&={1-\\cos^2(" angle ")} \\\\&=1-\\left("  
               (when (neg? numer) "-")
               "\\dfrac{" (.abs js/Math numer) "}{" denom "}\\right)^2 \\\\&=\\dfrac{" calc1-numer "}{" calc1-denom "}\\end{aligned}​​"))
     [:p]
     (tex (str "\\begin{aligned}\\sin(" angle ")&="
               (when-not (= quad "\\text{II}") "-")
               "\\sqrt{\\sin^2(" angle ")} \\\\&="
               (when-not (= quad "\\text{II}") "-")
               "\\sqrt{\\dfrac{" calc1-numer "}{" calc1-denom "}} \\\\&="
               (when-not (= quad "\\text{II}") "-")
               "\\dfrac{" 
               (if (float? (.sqrt js/Math calc1-numer))
                 (str "\\sqrt{" calc1-numer "}")
                 (.sqrt js/Math calc1-numer)) "}{" 
               (.sqrt js/Math calc1-denom) "}\\end{aligned}​"))]))