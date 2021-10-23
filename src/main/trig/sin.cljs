(ns trig.sin
  (:require [reagent.core :as r]
            ["katex" :as katex]
            [trig.editor :as editor]
            [sci.core :as sci]))

(defn abs [n]
  (.abs js/Math n))

(defn sin [n]
  (.sin js/Math n))

(defn cos [n]
  (.cos js/Math n))

(def pi js/Math.PI)

(defonce range-start (r/atom -8))

(defonce function-atom
  (r/atom (fn [x]
            (+
             (* 3
                (cos (+ (* 2 x)
                        (* pi 6))))
             4))))

(defonce points
  (r/atom {:max [0.5 12]
           :min [nil nil]
           :mid [0 7]}))

(defn x-point [x]
  (+ 150 (* x 18.75)))

(defn y-point [y]
  (- 175 (* y 18.75)))

(defn make-path [points]
  (str "M" (apply str (interpose " " (for [[x y] points]
                                       (str (x-point x) " " (y-point y)))))))

(def view-box-width 300)
(def view-box-height 326)

(def grid
  (fn []
    [:path {:d "M0 325V25M18.75 325V25M37.5 325V25M56.25 325V25M75 325V25M93.75 325V25M112.5 325V25M131.25 325V25M150 325V25M168.75 325V25M187.5 325V25M206.25 325V25M225 325V25M243.75 325V25M262.5 325V25M281.25 325V25M300 325V25M0 325h300M0 306.25h300M0 287.5h300M0 268.75h300M0 250h300M0 231.25h300M0 212.5h300M0 193.75h300M0 175h300M0 156.25h300M0 137.5h300M0 118.75h300M0 100h300M0 81.25h300M0 62.5h300M0 43.75h300M0 25h300"
            :stroke "#ffcc00"
            :stroke-width 2
            :opacity ".1"}]))

(defn axes []
  [:path {:d "M150 175H1.05
              M150 175h148.95
              M150 175v148.95
              M150 175V26.05"
          :stroke "#ffcc00"
          :stroke-linejoin "round"
          :stroke-linecap "round"
          :stroke-width 2}])

(defn arrows []
  [:path {:d "M7.05 169.4c-.35 2.1-4.2 5.25-5.25 5.6 1.05.35 4.9 3.5 5.25 5.6
              M294.45 180.6c.35-2.1 4.2-5.25 5.25-5.6-1.05-.35-4.9-3.5-5.25-5.6
              M156.35 31.3c-2.1-.35-5.25-4.2-5.6-5.25-.35 1.05-3.5 4.9-5.6 5.25
              M145.15 318.7c2.1.35 5.25 4.2 5.6 5.25.35-1.05 3.5-4.9 5.6-5.25"
          :fill "none"
          :stroke "#ffcc00"
          :stroke-linejoin "round"
          :stroke-linecap "round"
          :stroke-width 2}])

(defn ticks []
  [:path {:d "M168.75 180v-10M187.5 180v-10M206.25 180v-10M225 180v-10M243.75 180v-10M262.5 180v-10M281.25 180v-10M131.25 180v-10M112.5 180v-10M93.75 180v-10M75 180v-10M56.25 180v-10M37.5 180v-10M18.75 180v-10M145 156.25h10M145 137.5h10M145 118.75h10M145 100h10M145 81.25h10M145 62.5h10M145 43.75h10M145 193.75h10M145 212.5h10M145 231.25h10M145 250h10M145 268.75h10M145 287.5h10M145 306.25h10"
          :stroke "#ffcc00"}])

(defonce x-scale (r/atom "1"))

(defonce y-scale (r/atom "1"))

(defn x-slider
  [min max step]
  [:div (str "x-scale")
   [:input {:type      "range"
            :min       min
            :max       max
            :step      step
            :value     @x-scale
            :on-change #(reset! x-scale (-> % .-target .-value))}]
   [:span @x-scale]])

(defn y-slider
  [min max step]
  [:div (str "y-scale")
   [:input {:type      "range"
            :min       min
            :max       max
            :step      step
            :value     @y-scale
            :on-change #(reset! y-scale (-> % .-target .-value))}]
   [:span @y-scale]])

(defn calc-graph []
  (fn []
    (let [vals (fn [] [:path {:d (make-path (for [x (range @range-start 8 0.1)]
                                              [x (* @y-scale (#(@function-atom %)
                                                 ;x
                                                          ;(/ (* x js/Math.PI) 2)
                                                              (* x @x-scale)))]))
                              :stroke "blue"
                              :fill "none"
                              :stroke-width 2}])]
      [:svg {:width    "100%"
             :view-box (str "0 24 " view-box-width " " view-box-height)}
       [:g
        [grid]
        [arrows]
        [axes]
        [ticks] [vals]]])))

(defn reflection? [{[max-x max-y] :max
                    [mid-x mid-y] :mid
                    [min-x min-y] :min}]
  (cond (and mid-x min-x)
        (or (= min-x 0)
            (and (= 0 mid-x) (pos? min-x) (> mid-y min-y)))
        (and mid-x max-x)
        (> mid-y max-y)
        (and min-x max-x)
        (zero? min-x)))

(defn amplitude [{[max-x max-y] :max
                  [mid-x mid-y] :mid
                  [min-x min-y] :min :as w}]
  (let [abs-result
        (cond (and mid-x min-x) (abs (- mid-y min-y))
              (and mid-x max-x) (abs (- mid-y max-y))
              (and max-x min-x) (/ (abs (- max-y min-y)) 2))]
    (if (reflection? w) (- abs-result) abs-result)))

;; builds a map of the first 1000 divisions of pi, beginning with pi/2.
;; each decimal value is mapped to the TeX string representing its fraction.

(def fractions-of-pi
  (into {}
        (map (juxt (fn [n] (/ pi n))
                   (fn [n]
                     (str "\\dfrac{\\pi}{" n "}")))
             (range 2 1000))))

(defn fractions-of [x]
  (into {}
        (map (juxt (fn [n] (/ x n))
                   (fn [n]
                     (str "\\dfrac{" x "}{" n "}")))
             (range 2 50))))

(defn divisible? [n d] (= 0 (mod d n)))

(def simple-ratios
  (into {}
        (map (fn [[n d]] [(/ n d) (str "\\dfrac{" n "}{" d "}")])
             (for [n (range 1 100)
                   d (range 1 100)
                   :when (or (= 1 n)
                             (not (divisible? n d)))]
               [n d]))))

(defn period-mid [mid x]
  (cond
    (= (abs (- mid x)) 0.5) "\\pi"
    (= (abs (- mid x)) 0.75) "\\dfrac{2\\pi}{3}"
    (= (abs (- mid x)) 1.25) "\\dfrac{2\\pi}{5}"
    (int? (/ (* 2 pi) (* 4 (abs (- mid x)))))
    (/ (* 2 pi) (* 4 (abs (- mid x))))
    :else (or (get simple-ratios (/ (* 2 pi) (* 4 (abs (- mid x)))))
              (get fractions-of-pi (/ (* 2 pi) (* 4 (abs (- mid x)))))
              (str "\\dfrac{2\\pi}{" (* 4 (abs (- mid x))) "}"))))

(defn period [{[max-x max-y] :max
               [mid-x mid-y] :mid
               [min-x min-y] :min}]
  (cond (and mid-x min-x)
        (/ (* 2 pi) (* 4 (abs (- mid-x min-x))))
        (and mid-x max-x)
        (/ (* 2 pi) (* 4 (abs (- mid-x max-x))))
        (and max-x min-x)
        (/ (* 2 pi) (* 2 (abs (- max-x min-x))))))

(defn period-tex [{[max-x max-y] :max
                   [mid-x mid-y] :mid
                   [min-x min-y] :min}]
  (cond (and mid-x min-x)
        (period-mid mid-x min-x)
        (and mid-x max-x)
        (period-mid mid-x max-x)
        (and max-x min-x)
        (cond
          (= (abs (- max-x min-x)) 0.5) "2\\pi"
          (= (abs (- max-x min-x)) (/ pi 4)) "4"
          (int? (/ (* 2 pi) (* 2 (abs (- max-x min-x)))))
          (/ (* 2 pi) (* 2 (abs (- max-x min-x))))
          :else (or (get simple-ratios (/ (* 2 pi) (* 2 (abs (- max-x min-x)))))
                    (get fractions-of-pi (/ (* 2 pi) (* 2 (abs (- max-x min-x)))))
                    (str "\\dfrac{2\\pi}{" (* 2 (abs (- max-x min-x))) "}")))))

(defn x-shift-tex [{[max-x max-y] :max
                    [mid-x mid-y] :mid
                    [min-x min-y] :min}]
  (cond (and mid-x min-x)
        (cond
          (= min-x 0) ""
          (= mid-x pi) "-\\pi"
          (= mid-x (/ pi 2)) "-\\dfrac{\\pi}{2}"
          :else (if (pos? mid-x) (str "-" mid-x) (str "+" (abs mid-x))))
        (and mid-x max-x)
        (cond
          (= max-x 0) ""
          (= mid-x (* (/ 3 4) pi)) "-\\dfrac{3}{4}\\pi"
          (= mid-x (/ js/Math.PI 2)) "\\dfrac{\\pi}{2}"
          (= mid-x (- pi)) "+\\pi"
          (= mid-x (* -4 pi)) "+4\\pi"
          :else (if (pos? mid-x) (str "-" mid-x) (str "+" (abs mid-x))))
        (and max-x min-x)
        (cond
          (= min-x 0) ""
          (= max-x (- (/ pi 2))) "+\\dfrac{\\pi}{2}"
          (= max-x (/ (* 3 pi) 4)) "-\\dfrac{3\\pi}{4}"
          (= max-x (- (* 2 pi))) "+2\\pi"
          (= max-x pi) "-\\pi"
          :else (str (if (neg? max-x) (str "+" (abs max-x)) (str "-" max-x))))))

(defn x-shift [{[max-x max-y] :max
                [mid-x mid-y] :mid
                [min-x min-y] :min}]
  (cond (and mid-x min-x)
        (cond
          (= min-x 0) 0
          :else (- mid-x))
        (and mid-x max-x)
        (cond
          (= max-x 0) 0
          :else (- mid-x))
        (and max-x min-x)
        (cond
          (= min-x 0) 0
          :else (- max-x))))

(defn y-shift-tex [{[max-x max-y] :max
                    [mid-x mid-y] :mid
                    [min-x min-y] :min}]
  (cond (and mid-x min-x)
        (if (pos? mid-y) (str "+" mid-y) mid-y)
        (and mid-x max-x)
        (if (pos? mid-y) (str "+" mid-y) mid-y)
        (and max-x min-x)
        (if (pos? (- max-y (/ (abs (- max-y min-y)) 2)))
          (str "+" (- max-y (/ (abs (- max-y min-y)) 2)))
          (- max-y (/ (.abs js/Math (- max-y min-y)) 2)))))

(defn y-shift [{[max-x max-y] :max
                [mid-x mid-y] :mid
                [min-x min-y] :min}]
  (cond (and mid-x min-x)
        mid-y
        (and mid-x max-x)
        mid-y
        (and max-x min-x)
        (- max-y (/ (abs (- max-y min-y)) 2))))

(defn tex [text]
  [:span {:ref (fn [el]
                 (when el
                   (try
                     (katex/render text el (clj->js
                                            {:throwOnError false}))
                     (catch :default e
                       (js/console.warn "Unexpected KaTeX error" e)
                       (aset el "innerHTML" text)))))}])

(defn render-fn [w]
  (tex (str "\\large{f(x)="
            (if (= 1 (amplitude w)) "" (amplitude w))
            (if (= 0 (:mid-x w))
              "\\sin" "\\cos")
            "\\left({"
            (period-tex w)
            (if (contains? #{"" "+0" "-0"} (x-shift-tex w))
              "{x}"
              (str "(x\\red{" (x-shift-tex w) "})"))
            "}\\right)\\purple{"
            (if (= 0 (y-shift-tex w)) "" (y-shift-tex w)) "}}")))

(defonce !points (r/atom @points))

(defn eval-all [s]
  (try (sci/eval-string s {:classes {'js goog/global :allow :all}})
       (catch :default e
         (str e))))

(defn points-input []
  [:div
   [editor/editor (str @points) !points {:eval? true}]
   [:button {:on-click #(reset! points (eval-all
                                        (str "(def pi js/Math.PI)"
                                             (some-> @!points .-state .-doc str))))
             :style {:margin-top "1rem"}}
    "Eval"]])

(reset! function-atom
        (fn [x]
          (+
           (* (amplitude @points)
              (cos (* (period @points) (+ x (x-shift @points)))))
           (y-shift @points))))

(comment
  )