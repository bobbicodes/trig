(ns trig.law-of-sines
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

(defn sin [deg]
  (.sin js/Math (* deg (/ js/Math.PI 180))))

(defn asin [deg]
  (* (.asin js/Math deg) (/ 180 js/Math.PI)))

;[los/law-of-sines "m\\angle B" "\\text{I}" 3 8]

(defn los [triangle]
  (let [{:keys [angle1 angle2 angle3 line1 line2 line3]} triangle]
    (cond
      (and (pos? angle1) (pos? angle2) (pos? line2))
      (assoc triangle :line3 (* line2 (/ (sin angle2) (sin angle1))))
      (and (pos? angle1) (pos? angle2) (pos? line3))
      (assoc triangle :line2 (* line3 (/ (sin angle1) (sin angle2))))
      (and (pos? angle1) (pos? line1) (pos? line2))
      (assoc triangle :angle3 (asin (* (/ line1 line2) (sin angle1))))
      (and (pos? angle1) (pos? line2) (pos? line3))
      (assoc triangle :angle2 (asin (* (/ line3 line2) (sin angle1))))
      (and (pos? angle1) (pos? angle3) (pos? line1))
      (assoc triangle :line2 (* line1 (/ (sin angle1) (sin angle3))))
      (and (pos? angle1) (pos? angle3) (pos? line2))
      (assoc triangle :angle2 (* line2 (/ (sin angle3) (sin angle1))))
      (and (pos? angle1) (pos? angle3) (pos? line3))
      (assoc triangle :line2 (* line3 (/ (sin angle1) (sin angle3))))
      (and (pos? angle2) (pos? angle3) (pos? line1))
      (assoc triangle :line3 (* line1 (/ (sin angle2) (sin angle3))))
      (and (pos? angle2) (pos? angle3) (pos? line3))
      (assoc triangle :line1 (* line3 (/ (sin angle3) (sin angle2))))
      (and (pos? angle2) (pos? line1) (pos? line3))
      (assoc triangle :angle3 (asin (* (/ line1 line3) (sin angle2))))
      (and (pos? angle2) (pos? line2) (pos? line3))
      (assoc triangle :angle1 (asin (* (/ line2 line3) (sin angle2))))
      (and (pos? angle3) (pos? line1) (pos? line2))
      (assoc triangle :angle1 (asin (/ (* line2 (sin angle3))
                                       line1)))
      (and (pos? angle3) (pos? line1) (pos? line3))
      (assoc triangle :angle2 (asin (* (/ line3 line1) (sin angle3))))
      :else triangle)))

;; triangle query predicates

(defn sss? [{:keys [line1 line2 line3]}]
  (and (pos? line1) (pos? line2) (pos? line3)))

(defn sas? [{:keys [line1 line2 line3 angle1 angle2 angle3]}]
  (or
   (and (pos? angle1) (pos? line1) (pos? line3))
   (and (pos? angle2) (pos? line1) (pos? line2))
   (and (pos? angle3) (pos? line2) (pos? line3))))

(defn asa? [{:keys [line1 line2 line3 angle1 angle2 angle3]}]
  (or
   (and (pos? line1) (pos? angle1) (pos? angle2))
   (and (pos? line2) (pos? angle2) (pos? angle3))
   (and (pos? line3) (pos? angle1) (pos? angle3))))

(defn aas? [{:keys [line1 line2 line3 angle1 angle2 angle3]}]
  (or
   (and (pos? line1) (pos? angle1) (pos? angle2))
   (and (pos? line1) (pos? angle2) (pos? angle3))
   (and (pos? line2) (pos? angle1) (pos? angle3))
   (and (pos? line2) (pos? angle1) (pos? angle2))
   (and (pos? line3) (pos? angle1) (pos? angle2))
   (and (pos? line3) (pos? angle2) (pos? angle3))))

(defn law-of-sines [angle triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3 label1 label2 label3]} triangle]
    [:div
     (text "We can use the law of sines:")
     [:p]
     (tex (str "\\qquad \\blue{\\dfrac{\\sin(" angle ")}{"
               (cond (= angle "A") "a" (= angle "B") "b")
               "}} = \\green{\\dfrac{\\sin("
               (cond (= angle "A") "B" (= angle "B") "A") ")}{"
               (cond (= angle "A") "b" (= angle "B") "a") "}}"))
     [:p]
     (text "Multiply both sides by ") (tex (str "\\blue{" 
                                                (cond (= angle "A") "a" (= angle "B") "b")
                                                "}")) (text ":")
     [:p]
     (tex (str "\\qquad \\blue{\\sin("
               angle
               ")} = \\dfrac{\\blue{"
               (cond (= angle "A") "a" (= angle "B") "b")
               "}}{\\green{"
               (cond (= angle "A") "b" (= angle "B") "a")
               "}} \\green{\\sin("
               (cond (= angle "A") "B" (= angle "B") "A")
               ")}"))
     [:p]
     (text "Plug in the known values:​​")
     [:p]
     (tex (str "\\qquad \\blue{\\sin("
               angle
               ")} = \\dfrac{\\blue{" 
               (cond (and (pos? angle1) (pos? line2) (pos? line3)) line3
                     (and (pos? angle2) (pos? line2) (pos? line3)) line2)
                "}}{\\green{" 
               (cond (and (pos? angle1) (pos? line2) (pos? line3)) line2
                     (and (pos? angle2) (pos? line2) (pos? line3)) line3)
               "}} \\green{\\sin(" 
               (cond (pos? angle1) angle1
                     (pos? angle2) angle2) "^\\circ)}​"))
     [:p]
     (text "Evaluate the inverse sin to find ") (tex (str "\\blue""{" 
                                                          (cond (= angle "A") "A" (= angle "B") "B") 
                                                          "}")) (text ":")
     [:p]
     (tex (str "\\qquad \\blue{"
               angle
               "} =\\sin^{-1}\\left( \\dfrac{\\blue{"
               (cond (and (pos? angle1) (pos? line2) (pos? line3)) line3
                     (and (pos? angle2) (pos? line2) (pos? line3)) line2)
               "}}{\\green{"
               (cond (and (pos? angle1) (pos? line2) (pos? line3)) line2
                     (and (pos? angle2) (pos? line2) (pos? line3)) line3)
               "}} \\green{\\sin(" (cond (pos? angle1) angle1
                                         (pos? angle2) angle2) "^\\circ)}\\right)"))
     [:p]
     (tex (str "\\qquad \\blue{" angle "} \\approx \\blue{"
               (.round js/Math (asin (* (/ (cond (and (pos? angle1) (pos? line2) (pos? line3)) line3
                                                 (and (pos? angle2) (pos? line2) (pos? line3)) line2)
                                           (cond (and (pos? angle1) (pos? line2) (pos? line3)) line2
                                                 (and (pos? angle2) (pos? line2) (pos? line3)) line3))
                                        (sin (cond (pos? angle1) angle1
                                                   (pos? angle2) angle2))))) "^\\circ}"))]))
