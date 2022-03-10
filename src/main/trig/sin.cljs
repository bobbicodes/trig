(ns trig.sin
  (:require [reagent.core :as r]
            ["katex" :as katex]
            [trig.editor :as editor :refer [!points]]
            [sci.core :as sci]
            [trig.latex :as latex]
            [trig.ratio :as ratio]))

(defn abs [n]
  (.abs js/Math n))

(defn sin [n]
  (.sin js/Math n))

(defn cos [n]
  (.cos js/Math n))

(def pi js/Math.PI)

(defonce range-start (r/atom -13))

(reset! range-start -13)

(defonce function-atom
  (r/atom (fn [_] )))

(defonce points
  (r/atom {:max [2 2]
           :min [-2 -2]
           :mid [nil nil]}))

(defonce scale (r/atom :x))
(defonce x-scale (r/atom "1"))
(defonce y-scale (r/atom "1"))

(def view-box-width 300)
(def view-box-height 300)

(defn x-point [x]
  (+ (/ view-box-width 2) (* 
                           (if (= @scale :pi)
                             (/ x (* 0.5 pi)) x) 
                           (/ view-box-width 16))))

(defn y-point [y]
  (- (/ view-box-height 2) (* y (/ view-box-height 16))))

(defn make-path [points]
  (str "M" (apply str (interpose " " (for [[x y] points]
                                       (str x " " y))))))

(defn grid [size rows]
  [:g
   (for [x (range 0 (inc size) (/ size rows))]
     ^{:key x}
     [:line {:x1     x :y1     size :x2     x  :y2     0
             :stroke "#ffcc00"
             :stroke-width 1
             :opacity (if (= x (/ size 2)) 1 0.2)}])
   (for [y (range 0 (inc size) (/ size rows))]
     ^{:key y}
     [:line {:x1     0  :y1     y :x2     size  :y2     y
             :stroke "#ffcc00"
             :stroke-width 1
             :opacity (if (= y (/ size 2)) 1 0.22)}])])

(defn arrows [size]
  [:path {:d 
          (str "M6.2 " (- (/ size 2) 5.6)
               "c-.35 2.1-4.2 5.25-5.25 5.6 1.05.35 4.9 3.5 5.25 5.6"
               "M" (- size 6) " " (+ 5.6 (/ size 2))
               "c.35-2.1 4.2-5.25 5.25-5.6-1.05-.35-4.9-3.5-5.25-5.6"
               "M" (+ 5.6 (/ size 2)) " 6.2"
               "c-2.1-.35-5.25-4.2-5.6-5.25-.35 1.05-3.5 4.9-5.6 5.25"
               "M" (- (/ size 2) 5.6) " " (- size 6)
               "c2.1.35 5.25 4.2 5.6 5.25.35-1.05 3.5-4.9 5.6-5.25")
          :fill "none"
          :stroke "#ffcc00"
          :stroke-linejoin "round"
          :stroke-linecap "round"
          :stroke-width 1}])

(defn ticks [size rows]
  [:g
   (for [x (range 0 (- size (/ size rows)) (/ size rows))]
    ^{:key x}
     [:line {:x1     (+ x (/ size rows))
             :y1     (+ (/ (/ size rows) 3) (/ size 2))
             :x2     (+ x (/ size rows))
             :y2     (- (/ size 2) (/ (/ size rows) 3) )
             :stroke "#ffcc00"
             :stroke-width 1}])
   (for [y (range 0 (- size (/ size rows)) (/ size rows))]
     ^{:key y}
     [:line {:x1     (+ (/ (/ size rows) 3) (/ size 2))
             :y1     (+ y (/ size rows))
             :x2     (- (/ size 2) (/ (/ size rows) 3))
             :y2     (+ y (/ size rows))
             :stroke "#ffcc00"
             :stroke-width 1}])])

(defn x-slider [min max step]
  [:div (str "x")
   [:input {:type      "range"
            :min       min
            :max       max
            :step      step
            :value     @x-scale
            :on-change #(reset! x-scale (-> % .-target .-value))}]
   [:span @x-scale]])

(defn y-slider [min max step]
  [:div (str "y")
   [:input {:type      "range"
            :min       min
            :max       max
            :step      step
            :value     @y-scale
            :on-change #(reset! y-scale (-> % .-target .-value))}]
   [:span @y-scale]])

(defonce mouse-down? (r/atom false))
(defonce mouse-pos (r/atom [nil nil]))
(defonce drag (r/atom nil))

(defn coords [[x y]]
    (let [ints (cond
                 (and (< 7 x) (< 7 y)) [(- x 8) (- 8 y)]
                 (and (< 7 x) (< y 8)) [(- x 8) (- 8 y)]
                 (and (< x 8) (< 7 y)) [(+ -8 x) (- 8 y)]
                 (and (< x 8) (< y 8)) [(+ -8 x) (- 8 y)])]
      ((fn [[x y]] (if (= @scale :pi) [(* x (/ pi 2)) y] [x y])) 
       ints)))

(defn tex [text]
  [:span {:ref (fn [el]
                 (when el
                   (try
                     (katex/render text el (clj->js
                                            {:throwOnError false}))
                     (catch :default e
                       (js/console.warn "Unexpected KaTeX error" e)
                       (aset el "innerHTML" text)))))}])

(defn target 
  "Returns true if points x and y are within 0.5 unit from each other."
  [[x1 y1] [x2 y2]]
   (let [dx (abs (- x2 x1))
         dy (abs (- y2 y1))]
     (<= (+ dx dy) 0.5)))

(defn calc-graph []
  (fn []
    (let [{[max-x max-y] :max
           [mid-x mid-y] :mid
           [min-x min-y] :min} @points
          vals (fn [] 
                 [:path {:d (make-path (for [x (range @range-start 13 0.1)]
                                          [(x-point x) (y-point (* @y-scale (#(@function-atom %)
                                             (* x @x-scale))))]))
                         :stroke         "blue"
                         :fill           "none"
                         :pointer-events "none"
                         :stroke-width   2}])]
      [:div [:svg {:width    700
                   :view-box (str "0 0 " view-box-width " " view-box-height)
                   :style    {:cursor (when (or (target (coords @mouse-pos) [max-x max-y])
                                                (target (coords @mouse-pos) [mid-x mid-y])
                                                (target (coords @mouse-pos) [min-x min-y])
                                                (target (coords @mouse-pos) [(/ max-x (* 0.5 pi)) max-y])
                                                (target (coords @mouse-pos) [(/ mid-x (* 0.5 pi)) mid-y])
                                                (target (coords @mouse-pos) [(/ min-x (* 0.5 pi)) min-y])) "move")}}
             [:g [grid view-box-width 16] [arrows view-box-width] [ticks view-box-width 16] [vals]
              [:g
               (when (and min-x min-y)
                 [:circle {:r    (if 
                                  (or
                                   (target (coords @mouse-pos) [min-x min-y])
                                   (target (coords @mouse-pos) [(/ min-x (* 0.5 pi)) min-y])) 4 3) 
                           :cx   (x-point (* @x-scale min-x))
                           :cy   (y-point (* @y-scale min-y))
                           :pointer-events "none"
                           :fill "green"}])
               (when (and mid-x mid-y)
                 [:circle {:r    (if (or 
                                      (target (coords @mouse-pos) [mid-x mid-y])
                                      (target (coords @mouse-pos) [(/ mid-x (* 0.5 pi)) mid-y])) 4 3)
                           :cx   (x-point (* @x-scale mid-x))
                           :cy   (y-point (* @y-scale mid-y))
                            :pointer-events "none"
                           :fill "green"}])
               (when (and max-x max-y)
                 [:circle {:r    (if 
                                  (or
                                   (target (coords @mouse-pos) [max-x max-y])
                                   ;; if x-scale is in units of pi
                                   (target (coords @mouse-pos) [(/ max-x (* 0.5 pi)) max-y])) 4 3)
                           :cx   (x-point (* @x-scale max-x))
                           :cy   (y-point (* @y-scale max-y))
                            :pointer-events "none"
                           :fill "green"}])]]
             (if (= @scale :pi)
               [:g
                [:text {:transform "scale(0.6) translate(304,276)"
                        :fill      "#ffcc00"} (str (let [v (.floor js/Math (* @x-scale 1))]
                                                     (if (= 1 v) "" v))
                                                   "π")]
                [:text {:transform "scale(0.6) translate(362,276)"
                        :fill      "#ffcc00"} (str (.floor js/Math (* @x-scale 2)) "π")]
                [:text {:transform "scale(0.6) translate(426,276)"
                        :fill      "#ffcc00"} (str (.floor js/Math (* @x-scale 3)) "π")]
                [:text {:transform "scale(0.6) translate(266,190)"
                        :fill      "#ffcc00"} (.floor js/Math (* @y-scale 2))]
                [:text {:transform "scale(0.6) translate(266,130)"
                        :fill      "#ffcc00"} (.floor js/Math (* @y-scale 4))]
                [:text {:transform "scale(0.6) translate(266,68)"
                        :fill      "#ffcc00"} (.floor js/Math (* @y-scale 6))]]
               [:g
                [:text {:transform "scale(0.6) translate(304,276)"
                        :fill      "#ffcc00"} (.floor js/Math (* @x-scale 2))]
                [:text {:transform "scale(0.6) translate(368,276)"
                        :fill      "#ffcc00"} (.floor js/Math (* @x-scale 4))]
                [:text {:transform "scale(0.6) translate(432,276)"
                        :fill      "#ffcc00"} (.floor js/Math (* @x-scale 6))]
                [:text {:transform "scale(0.6) translate(266,190)"
                        :fill      "#ffcc00"} (.floor js/Math (* @y-scale 2))]
                [:text {:transform "scale(0.6) translate(266,130)"
                        :fill      "#ffcc00"} (.floor js/Math (* @y-scale 4))]
                [:text {:transform "scale(0.6) translate(266,68)"
                        :fill      "#ffcc00"} (.floor js/Math (* @y-scale 6))]])
             ;; mouse tracking grid
             (let [size (/ view-box-width 16)]
               (for [x (range 0 17 0.5)
                     y (range 0 17 0.5)]
                 ^{:key [x y]}
                 [:rect {:width          size
                         :height         size
                         :x              (- (* x size) (/ size 2))
                         :y              (- (* y size) (/ size 2))
                         :on-mouse-down  (fn [] 
                                           (reset! mouse-down? true)
                                           (when (or (target (coords @mouse-pos) [max-x max-y])
                                                     (target (coords @mouse-pos) [(/ max-x (* 0.5 pi)) max-y]))
                                             (reset! drag :max))
                                           (when (or (target (coords @mouse-pos) [mid-x mid-y])
                                                     (target (coords @mouse-pos) [(/ mid-x (* 0.5 pi)) mid-y]))
                                             (reset! drag :mid))
                                           (when (or (target (coords @mouse-pos) [min-x min-y])
                                                     (target (coords @mouse-pos) [(/ min-x (* 0.5 pi)) min-y]))
                                             (reset! drag :min)))
                         :on-mouse-over  (fn []
                                           (reset! mouse-pos [x y])
                                           (when (= :max @drag)
                                             (swap! points assoc :max (coords [x y]))
                                             (editor/update-editor! (str @points)))
                                           (when (= :mid @drag)
                                             (swap! points assoc :mid (coords [x y]))
                                             (editor/update-editor! (str @points)))
                                           (when (= :min @drag)
                                             (swap! points assoc :min (coords [x y]))
                                             (editor/update-editor! (str @points))))
                         :on-mouse-up    (fn []
                                           (reset! mouse-down? false)
                                           (reset! drag nil))
                         :visibility     "hidden"
                         :pointer-events "all"}]))]
                         #_[:p (str @mouse-pos)]
                         #_[:p (str (coords @mouse-pos))]])))

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

(defn period-mid [mid x]
  (cond
    (= (abs (- mid x)) (/ pi 2)) ""
    (= (abs (- mid x)) 0.5) "\\pi"
    (= (abs (- mid x)) 0.75) "\\dfrac{2\\pi}{3}"
    (= (abs (- mid x)) 1.25) "\\dfrac{2\\pi}{5}"
    (int? (/ (* 2 pi) (* 4 (abs (- mid x)))))
    (/ (* 2 pi) (* 4 (abs (- mid x))))
    :else (or (get ratio/simple-ratios (/ (* 2 pi) (* 4 (abs (- mid x)))))
              (get ratio/fractions-of-pi (/ (* 2 pi) (* 4 (abs (- mid x)))))
              (str "\\dfrac{2\\pi}{" (* 4 (abs (- mid x))) "}"))))

(defn period [{[max-x _] :max
               [mid-x _] :mid
               [min-x _] :min}]
  (cond (and mid-x min-x)
        (/ (* 2 pi) (* 4 (abs (- mid-x min-x))))
        (and mid-x max-x)
        (/ (* 2 pi) (* 4 (abs (- mid-x max-x))))
        (and max-x min-x)
        (/ (* 2 pi) (* 2 (abs (- max-x min-x))))))

(defn period-tex [{[max-x _] :max
                   [mid-x _] :mid
                   [min-x _] :min}]
  (cond (and mid-x min-x)
        (period-mid mid-x min-x)
        (and mid-x max-x)
        (period-mid mid-x max-x)
        (and max-x min-x)
        (cond
          (= (abs (- max-x min-x)) 0.5) "2\\pi"
          (= (abs (- max-x min-x)) pi) ""
          (= (abs (- max-x min-x)) (/ pi 4)) "4"
          (int? (/ (* 2 pi) (* 2 (abs (- max-x min-x)))))
          (/ (* 2 pi) (* 2 (abs (- max-x min-x))))
          :else (or (get ratio/simple-ratios 
                         (/ (* 2 pi) (* 2 (abs (- max-x min-x)))))
                    (get ratio/fractions-of-pi 
                         (/ (* 2 pi) (* 2 (abs (- max-x min-x)))))
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
          (= max-x (* 2 pi)) "-2\\pi"
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
  (cond (and mid-x min-x) mid-y
        (and mid-x max-x) mid-y
        (and max-x min-x) (- max-y (/ (abs (- max-y min-y)) 2))))

(defonce trig-fn (r/atom nil))
(defonce render-mode (r/atom :a))

(defn render-a [w]
  (str (if (= 1 (amplitude w)) "" (amplitude w))
       @trig-fn
       "\\left({" (period-tex w)
       (if (contains? #{"" "+0" "-0"} (x-shift-tex w))
         "{x}" (str "(x\\red{" (x-shift-tex w) "})"))
       "}\\right)\\purple{"
       (if (= 0 (y-shift-tex w)) "" (y-shift-tex w)) "}"))

(defn render-b [w]
  (let [p (or (get (ratio/ratios 100) (period w))
              (/ (get ratio/fracs-of-7pi (period w)) 7)
              (/ (get ratio/fracs-of-2pi (period w)) 2)
              (get ratio/fracs-of-pi (period w))
              )]
    (str (if (= 1 (amplitude w)) "" (amplitude w))
         @trig-fn
         "\\left(" (period-tex w) (if (neg? (x-shift w)) "x-" "x+")
         (if (contains? #{"" "+0" "-0"} (x-shift-tex w))
           "{x}" 
           (or (get ratio/fractions-of-pi (/ pi (/ p (x-shift w))))
               (get ratio/fractions-of-2pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-3pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-4pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-5pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-6pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-7pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-8pi (abs (* (period w) (x-shift w))))
               (get ratio/fractions-of-9pi (abs (* (period w) (x-shift w))))
               (str "\\dfrac{\\pi}{" (/ p (x-shift w)) "}")))
         "\\right)\\purple{"
         (if (= 0 (y-shift-tex w)) "" (y-shift-tex w)) "}")))

(defn render-fn [w]
  (if (= :a @render-mode)
    (render-a w)
    (render-b w)))

(comment
  (render-fn @points)
   (get ratio/fractions-of-4pi (/ (* 4 pi) 3))
  (get ratio/fractions-of-4pi (* (period @points) (x-shift @points)))
  (/ pi (* (period @points) (x-shift @points)))
  (get (ratio/ratios 100) (period @points))
(/ (* 3 pi) 2)
(get ratio/fractions-of-7pi (abs (/ (* -7 pi) 9)))
(period @points)
  (x-shift @points)

  (get (ratio/ratios 100) (period {:max [pi 6]
                             :min [(/ (* -3 pi) 4) 2]
                             :mid [nil nil]}))

  (period-tex {:max [pi 6]
               :min [(/ (* -3 pi) 4) 2]
               :mid [nil nil]})

  (x-shift-tex {:max [pi 6]
                :min [(/ (* -3 pi) 4) 2]
                :mid [nil nil]})

  (render-fn {:max [pi 6]
              :min [(/ (* -3 pi) 4) 2]
              :mid [nil nil]})
  )

(defn eval-all [s]
  (try (sci/eval-string s {:classes {'js goog/global :allow :all}})
       (catch :default e
         (str e))))

(defn points-input []
  [:div
   [editor/editor (str @points) !points {:eval? true}]
   [:div.flex-container
    [:div.flex-item
     [:button {:on-click #(reset! trig-fn "\\sin")} (tex "\\sin")]
     [:button {:on-click #(reset! trig-fn "\\cos")} (tex "\\cos")]
     ]
    [:div.flex-item
     [:button {:on-click #(reset! points (eval-all (str "(def pi js/Math.PI)"
                                                        (some-> @!points .-state .-doc str))))}
      "Eval"]]
    [:div.flex-item
     [:button {:on-click (fn [] (swap! render-mode #(if (= :a %) :b :a)))} 
      (if (= :a @render-mode)
        (tex "\\tiny{\\dfrac{1}{2}x+\\dfrac{\\pi}{2}}")
        (tex "\\tiny{\\dfrac{1}{2}(x+\\pi)}"))]]]])

(reset! function-atom
        (fn [x]
          (+
           (* (amplitude @points)
              (if (= @trig-fn "\\sin")
                    (sin (* (period @points) (+ x (x-shift @points))))
                    (cos (* (period @points) (+ x (x-shift @points)))))
              )
           (y-shift @points))))
