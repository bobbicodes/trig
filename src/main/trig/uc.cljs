(ns trig.uc
  (:require [trig.latex :as latex :refer [render-letter letters]]
            [trig.math :refer [sq pi sqrt sin cos tan]]))

(defn grid []
  [:g
   [:path {:fill "none" :stroke "#ffcc00" :d "M 16.6667,425 L 16.6667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 33.3333,425 L 33.3333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 50,425 L 50,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 66.6667,425 L 66.6667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 83.3333,425 L 83.3333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 100,425 L 100,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 116.667,425 L 116.667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 133.333,425 L 133.333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 150,425 L 150,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 166.667,425 L 166.667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 183.333,425 L 183.333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 200,425 L 200,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 216.667,425 L 216.667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 233.333,425 L 233.333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 250,425 L 250,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 266.667,425 L 266.667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 283.333,425 L 283.333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 300,425 L 300,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 316.667,425 L 316.667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 333.333,425 L 333.333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 350,425 L 350,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 366.667,425 L 366.667,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 383.333,425 L 383.333,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 400,425 L 400,25" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,408.333 L 400,408.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,391.667 L 400,391.667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,375 L 400,375" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,358.333 L 400,358.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,341.667 L 400,341.667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,325 L 400,325" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,308.333 L 400,308.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,291.667 L 400,291.667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,275 L 400,275" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,258.333 L 400,258.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,241.667 L 400,241.667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,225 L 400,225" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,208.333 L 400,208.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,191.667 L 400,191.667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,175 L 400,175" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,158.333 L 400,158.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,141.667 L 400,141.667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,125 L 400,125" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,108.333 L 400,108.333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,91.6667 L 400,91.6667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,75 L 400,75" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,58.3333 L 400,58.3333" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,41.6667 L 400,41.6667" :stroke-width 2 :opacity 0.1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 0,25 L 400,25" :stroke-width 2 :opacity 0.1}]])

(defn arrows []
  [:g
   [:path {:fill "none" :stroke "#ffcc00" :d "M -3.45,230.6 C -3.1,228.5 0.75,225.35 1.8,225 C 0.75,224.65 -3.1,221.5 -3.45,219.4" :transform "rotate(180 1.8000000000000398 225)" :stroke-width 2 :opacity 1 :stroke-linejoin "round" :stroke-linecap "round"}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 394.45,230.6 C 394.8,228.5 398.65,225.35 399.7,225 C 398.65,224.65 394.8,221.5 394.45,219.4"  :stroke-width "2" :opacity 1 :stroke-linejoin "round" :stroke-linecap "round"}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 195.5,429.55 C 195.85,427.45 199.7,424.3 200.75,423.95 C 199.7,423.6 195.85,420.45 195.5,418.35" :transform "rotate(90 200.75000000000003 423.95)" :stroke-width "2" :opacity 1 :stroke-linejoin "round" :stroke-linecap "round"}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 195.5,31.65 C 195.85,29.55 199.7,26.4 200.75,26.05 C 199.7,25.7 195.85,22.55 195.5,20.45" :transform "rotate(-90 200.75000000000003 26.05000000000001)" :stroke-width "2" :opacity 1 :stroke-linejoin "round" :stroke-linecap "round"}]])

(defn ticks []
  [:g
   [:path {:fill "none" :stroke "#ffcc00" :d "M 283.333,230 L 283.333,220" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 116.667,230 L 116.667,220" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 195,141.667 L 205,141.667" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 195,308.333 L 205,308.333" :stroke-width 1 :opacity 1}]])

(defn arcs []
  [:g [:path {:fill "#ffcc00" :stroke "#a24d61" :d "M 141.667,225 A 58.3333,58.3333 0 0,0 158.752,266.248" :stroke-width "2" :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "0" :fill-opacity "0"}]
   [:path {:fill "#ffcc00" :stroke "#a24d61" :d "M 138.333,225 A 61.6667,61.6667 0 0,0 156.395,268.605" :stroke-width "2" :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "0" :fill-opacity "0"}]])

(defn axes []
  [:g
   [:path {:fill "none" :stroke "#ffcc00" 
           :d "M 368,225 H 32,225" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" 
           :d "M 200,393 V 200,58" :stroke-width 1 :opacity 1}]])

(defn circle [cx cy]
  [:ellipse {:cx cx :cy cy :rx 166.66666666666669 :ry 166.66666666666669 
             :fill "#29abca" :stroke "#29abca" :stroke-width "3" 
             :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "0" :fill-opacity "0"}])

(defn hypotenuse [x y]
 [:path {:fill "none" :stroke "#ca337c" 
         :d (str "M 200,225 L " x "," y) :stroke-width "2" :clip-path "url(#clip-1)"}])

(defn side [x1 y1 x2 y2]
  [:path {:fill "none" :stroke "#ca337c" 
          :d (str "M " x1 "," y1 " L " x2 "," y2)  :stroke-width 2 :clip-path "url(#clip-1)"}])

(defn point [cx cy]
 [:ellipse {:cx cx :cy cy :rx 3 :ry 3 
            :fill "#7AF745" :stroke "#7AF745"  :stroke-width "3" :clip-path "url(#clip-1)"}])

(defn arc [x y]
  [:path {:fill "#ffcc00" :stroke "#ffcc00"
          :d (str "M 266.667,225 A 66.6667,66.6667 0 1,0 " x "," y) :stroke-width "2"
          :stroke-opacity 1 :fill-opacity "0"}])

{:vertices ["A" "B" "C"],:lines [5 nil nil],:angles [(/ pi 3) (/ pi 2) nil]}


(defn uc [triangle]
  (fn [{[side1 side2 side3] :sides
        [angle1 angle2 angle3] :angles
        [label1 label2 label3] :vertices}]
    (let [x (+ 200 (* 166.66666666666669 side1))
          y (- 225 (* 166.66666666666669 side2))]
      [:svg {:width "100%" :view-box "0 0 425 425"}
     ;[grid]
     ;[arrows]
       [axes]
     ;[ticks]
       [circle 200 225]
       [hypotenuse x y]
       [side x 225 x y]
       [point x y]
       ;; label (angle at origin)
       [:path {:fill "#ffcc00"
               :transform (str "translate(170,215) scale(" 0.03 "," -0.03 ")")
               :d (:theta letters)}]
     ;[arcs]
       #_[arc 152.86 272.14]])))

(defn uc-theta [angle]
  (fn [angle]
    (let [x (+ 200 (* 166.66666666666669 (cos angle)))
          y (- 225 (* 166.66666666666669 (sin angle)))]
      [:svg {:width "100%" :view-box "0 0 425 425"}
     ;[grid]
     ;[arrows]
       [axes]
     ;[ticks]
       [circle 200 225]
       [hypotenuse x y]
       [side x 225 x y]
       [point x y]
       ;; label (angle at origin)
       [:path {:fill "#ffcc00"
               :transform (str "translate(170,215) scale(" 0.03 "," -0.03 ")")
               :d (:theta letters)}]
     ;[arcs]
       #_[arc 152.86 272.14]])))

(defn uc-1 
  "Triangle with angle pi/6"
  []
  [:div
   [:svg {:width "72%" :view-box "0 25 265 240"}
    [:defs [:clip-path {:id "a"} [:path {:d "M0 25h240v240H0z"}]]
     [:path {:stroke-width "1" :id "E1-MJMATHI-3C0" :d "M132 -11Q98 -11 98 22V33L111 61Q186 219 220 334L228 358H196Q158 358 142 355T103 336Q92 329 81 318T62 297T53 285Q51 284 38 284Q19 284 19 294Q19 300 38 329T93 391T164 429Q171 431 389 431Q549 431 553 430Q573 423 573 402Q573 371 541 360Q535 358 472 358H408L405 341Q393 269 393 222Q393 170 402 129T421 65T431 37Q431 20 417 5T381 -10Q370 -10 363 -7T347 17T331 77Q330 86 330 121Q330 170 339 226T357 318T367 358H269L268 354Q268 351 249 275T206 114T175 17Q164 -11 132 -11Z"}]
     [:path {:stroke-width "1" :id "E1-MJMAIN-36" :d "M42 313Q42 476 123 571T303 666Q372 666 402 630T432 550Q432 525 418 510T379 495Q356 495 341 509T326 548Q326 592 373 601Q351 623 311 626Q240 626 194 566Q147 500 147 364L148 360Q153 366 156 373Q197 433 263 433H267Q313 433 348 414Q372 400 396 374T435 317Q456 268 456 210V192Q456 169 451 149Q440 90 387 34T253 -22Q225 -22 199 -14T143 16T92 75T56 172T42 313ZM257 397Q227 397 205 380T171 335T154 278T148 216Q148 133 160 97T198 39Q222 21 251 21Q302 21 329 59Q342 77 347 104T352 209Q352 289 347 316T329 361Q302 397 257 397Z"}]]
    [:path {:fill "none" :stroke "#ffcc00" :d "M20.87 265V25M229.565 265V25M0 244.13h240M0 35.435h240" :stroke-width 2 :opacity 0.1}]
    [:path {:fill "none" :stroke "#000" :d "M7.05 238.53c-.35 2.1-4.2 5.25-5.25 5.6 1.05.35 4.9 3.5 5.25 5.6" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13H1.05" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M234.45 249.73c.35-2.1 4.2-5.25 5.25-5.6-1.05-.35-4.9-3.5-5.25-5.6" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13h218.08" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M15.3 258.7c2.1.35 5.25 4.2 5.6 5.25.35-1.05 3.5-4.9 5.6-5.25" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13v19.82" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M26.5 31.3c-2.1-.35-5.25-4.2-5.6-5.25-.35 1.05-3.5 4.9-5.6 5.25" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13V26.05" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M125.217 249.13v-10M229.565 249.13v-10M15.87 139.783h10M15.87 35.435h10"}]
    [:ellipse {:cx "20.87" :cy "244.13" :rx "208.696" :ry "208.696" :fill "none" :stroke "#11accd" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    [:path {:stroke "#ca337c" :d "M51.322 244.13a30.453 30.453 0 0 0-4.08-15.226M201.605 230.756H188.23M188.23 244.13v-13.374" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0" :fill "none"}]
    [:path {:stroke "#ca337c" :d "M20.87 244.13l180.735-104.347V244.13z" :stroke-width "2" :stroke-dasharray "0" :fill "none"}]
    [:ellipse {:cx "20.87" :cy "244.13" :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    [:ellipse {:cx "201.605" :cy "139.783" :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    [:ellipse {:cx "201.605" :cy "244.13" :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    [:path {:transform (str "translate(" 205 "," 130 ")" "scale(" 0.018 "," (- 0.018) ")")
            :fill "#ffcc00" :d (:A letters)}]
    [:g {:stroke "currentColor" :fill "currentColor" :stroke-width "0" :transform "matrix(1 0 0 -1 0 0)"}
     [:g {:transform (str "translate(" 70 "," -232 ")" "scale(" 0.013 "," 0.013 ")")}
      [:rect {:stroke "none" :width "693" :height "60" :x "0" :y "220"}]
      [:use {:xlink-href "#E1-MJMATHI-3C0" :x "60" :y "676"}]
      [:use {:xlink-href "#E1-MJMAIN-36" :x "96" :y "-687"}]]]]])

(defn uc-2 
  "Triangle with angle pi/3"
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices}]
  [:div
   [:svg {:width "100%" :view-box "0 25 265 240"}
    [:defs [:clip-path {:id "a"} [:path {:d "M0 25h240v240H0z"}]]
     [:path {:stroke-width "1" :id "E1-MJMATHI-3C0" :d "M132 -11Q98 -11 98 22V33L111 61Q186 219 220 334L228 358H196Q158 358 142 355T103 336Q92 329 81 318T62 297T53 285Q51 284 38 284Q19 284 19 294Q19 300 38 329T93 391T164 429Q171 431 389 431Q549 431 553 430Q573 423 573 402Q573 371 541 360Q535 358 472 358H408L405 341Q393 269 393 222Q393 170 402 129T421 65T431 37Q431 20 417 5T381 -10Q370 -10 363 -7T347 17T331 77Q330 86 330 121Q330 170 339 226T357 318T367 358H269L268 354Q268 351 249 275T206 114T175 17Q164 -11 132 -11Z"}]
     [:path {:stroke-width "1" :id "E1-MJMAIN-33" :d "M127 463Q100 463 85 480T69 524Q69 579 117 622T233 665Q268 665 277 664Q351 652 390 611T430 522Q430 470 396 421T302 350L299 348Q299 347 308 345T337 336T375 315Q457 262 457 175Q457 96 395 37T238 -22Q158 -22 100 21T42 130Q42 158 60 175T105 193Q133 193 151 175T169 130Q169 119 166 110T159 94T148 82T136 74T126 70T118 67L114 66Q165 21 238 21Q293 21 321 74Q338 107 338 175V195Q338 290 274 322Q259 328 213 329L171 330L168 332Q166 335 166 348Q166 366 174 366Q202 366 232 371Q266 376 294 413T322 525V533Q322 590 287 612Q265 626 240 626Q208 626 181 615T143 592T132 580H135Q138 579 143 578T153 573T165 566T175 555T183 540T186 520Q186 498 172 481T127 463Z"}]]
    [:path {:fill "none" :stroke "#ffcc00" :d "M20.87 265V25M229.565 265V25M0 244.13h240M0 35.435h240" :stroke-width 2 :opacity 0.1}]
    [:path {:fill "none" :stroke "#000" :d "M7.05 238.53c-.35 2.1-4.2 5.25-5.25 5.6 1.05.35 4.9 3.5 5.25 5.6" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13H1.05" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M234.45 249.73c.35-2.1 4.2-5.25 5.25-5.6-1.05-.35-4.9-3.5-5.25-5.6" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13h218.08" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M15.3 258.7c2.1.35 5.25 4.2 5.6 5.25.35-1.05 3.5-4.9 5.6-5.25" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13v19.82" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M26.5 31.3c-2.1-.35-5.25-4.2-5.6-5.25-.35 1.05-3.5 4.9-5.6 5.25" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13V26.05" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M125.217 249.13v-10M229.565 249.13v-10M15.87 139.783h10M15.87 35.435h10"}]
    ;; main circle - radius 208.696
    [:ellipse {:cx "20.87" :cy "244.13" :rx "208.696" :ry "208.696" :fill "none" :stroke "#11accd" :stroke-width "2" :stroke-dasharray "0"}]
    ;; right-angle box and inner arc
    [:path {:stroke "#ca337c" 
            :d (str "M38.422 244.13a17.552 17.552 0 0 0-8.776-15.2M125.217 230.756h-13.374M111.843 244.13v-13.374") :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0" :fill "none"}]
    ;; triangle
    [:path {:stroke "#ca337c" 
            :d (str "M20.87 244.13L" 
                    (+ 20.87 (* 208.696 side2)) " " 
                    (- 244.13 (* (- 244.13 35.435) (sqrt (- (sq side3) (sq side2)))))
                    "V244.13z") :stroke-width "2" :stroke-dasharray "0" :fill "none"}]
    ;; point at origin
    [:ellipse {:cx "20.87" :cy "244.13" :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    ;; point on circle
    [:ellipse {:cx (+ 20.87 (* 208.696 side2)) 
               :cy (- 244.13 (* (- 244.13 35.435) (sqrt (- (sq side3) (sq side2)))))
               :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    ;; point on line
    [:ellipse {:cx (+ 20.87 (* 208.696 side2)) :cy 244.13 :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    ;; label (point on circle)
    [:path {:transform (str "translate(" 125.217 "," 55 ")" "scale(" 0.018 "," (- 0.018) ")")
            :fill "#ffcc00" :d (:A letters)}]
    ;; label (angle)
    [:g {:stroke "currentColor" :fill "currentColor" :stroke-width "0" :transform "matrix(1 0 0 -1 0 0)"}
     [:g {:transform (str "translate(" 50 "," -232 ")" "scale(" 0.013 "," 0.013 ")")}
      [:rect {:stroke "none" :width "693" :height "60" :x "0" :y "220"}]
      [:use {:xlink-href "#E1-MJMATHI-3C0" :x "60" :y "676"}]
      [:use {:xlink-href "#E1-MJMAIN-33" :x "96" :y "-687"}]]]]])

(defn uc-3
  "Triangle with angle pi/4"
  [{[side1 side2 side3] :sides
    [angle1 angle2 angle3] :angles
    [label1 label2 label3] :vertices}]
  [:div
   [:svg {:width "100%" :view-box "0 25 265 240"}
    [:defs [:clip-path {:id "a"} [:path {:d "M0 25h240v240H0z"}]]
     [:path {:stroke-width "1" :id "E1-MJMATHI-3C0" :d "M132 -11Q98 -11 98 22V33L111 61Q186 219 220 334L228 358H196Q158 358 142 355T103 336Q92 329 81 318T62 297T53 285Q51 284 38 284Q19 284 19 294Q19 300 38 329T93 391T164 429Q171 431 389 431Q549 431 553 430Q573 423 573 402Q573 371 541 360Q535 358 472 358H408L405 341Q393 269 393 222Q393 170 402 129T421 65T431 37Q431 20 417 5T381 -10Q370 -10 363 -7T347 17T331 77Q330 86 330 121Q330 170 339 226T357 318T367 358H269L268 354Q268 351 249 275T206 114T175 17Q164 -11 132 -11Z"}]
     [:path {:stroke-width "1" :id "E1-MJMAIN-34" :d "M462 0Q444 3 333 3Q217 3 199 0H190V46H221Q241 46 248 46T265 48T279 53T286 61Q287 63 287 115V165H28V211L179 442Q332 674 334 675Q336 677 355 677H373L379 671V211H471V165H379V114Q379 73 379 66T385 54Q393 47 442 46H471V0H462ZM293 211V545L74 212L183 211H293Z"}]]
    [:path {:fill "none" :stroke "#ffcc00" :d "M20.87 265V25M229.565 265V25M0 244.13h240M0 35.435h240" :stroke-width 2 :opacity 0.1}]
    [:path {:fill "none" :stroke "#000" :d "M7.05 238.53c-.35 2.1-4.2 5.25-5.25 5.6 1.05.35 4.9 3.5 5.25 5.6" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13H1.05" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M234.45 249.73c.35-2.1 4.2-5.25 5.25-5.6-1.05-.35-4.9-3.5-5.25-5.6" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13h218.08" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M15.3 258.7c2.1.35 5.25 4.2 5.6 5.25.35-1.05 3.5-4.9 5.6-5.25" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13v19.82" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M26.5 31.3c-2.1-.35-5.25-4.2-5.6-5.25-.35 1.05-3.5 4.9-5.6 5.25" :stroke-width "2" :stroke-linejoin "round" :stroke-linecap "round"}]
    [:path {:fill "none" :stroke "#000" :d "M20.87 244.13V26.05" :stroke-width "2"}]
    [:path {:fill "none" :stroke "#000" :d "M125.217 249.13v-10M229.565 249.13v-10M15.87 139.783h10M15.87 35.435h10"}]
    ;; main circle - radius 208.696
    [:ellipse {:cx "20.87" :cy "244.13" :rx "208.696" :ry "208.696" :fill "none" :stroke "#11accd" :stroke-width "2" :stroke-dasharray "0"}]
    ;; inner arc
    [:path {:stroke "#ca337c"
            :d (str "M38.422 244.13a17.552 17.552 0 0 0-5-14") :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0" :fill "none"}]
    ;; right-angle box
    [:path {:stroke "#ca337c"
            :d (str "M167.5 230.756h-13.374M155 244.13v-13.374") :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0" :fill "none"}]
    ;; triangle
    [:path {:stroke "#ca337c"
            :d (str "M20.87 244.13L"
                    (+ 20.87 (* 208.696 side2)) " "
                    (- 244.13 (* (- 244.13 35.435) (sqrt (- (sq side3) (sq side2)))))
                    "V244.13z") :stroke-width "2" :stroke-dasharray "0" :fill "none"}]
    ;; point at origin
    [:ellipse {:cx "20.87" :cy "244.13" :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    ;; point on circle
    [:ellipse {:cx (+ 20.87 (* 208.696 side2))
               :cy (- 244.13 (* (- 244.13 35.435) (sqrt (- (sq side3) (sq side2)))))
               :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    ;; point on line
    [:ellipse {:cx (+ 20.87 (* 208.696 side2)) :cy 244.13 :rx "3" :ry "3" :fill "#ca337c" :stroke "#ca337c" :stroke-width "2" :clip-path "url(#a)" :stroke-dasharray "0"}]
    ;; label (point on circle)
    [:path {:transform (str "translate(" 
                            (+ 20.87 (* 208.696 side2)) "," 
                            (- 235 (* (- 244.13 35.435) (sqrt (- (sq side3) (sq side2)))))
                            ")" "scale(" 0.018 "," (- 0.018) ")")
            :fill "#ffcc00" :d (:A letters)}]
    ;; label (angle)
    [:g {:stroke "currentColor" :fill "currentColor" :stroke-width "0" :transform "matrix(1 0 0 -1 0 0)"}
     [:g {:transform (str "translate(" 50 "," -232 ")" "scale(" 0.013 "," 0.013 ")")}
      [:rect {:stroke "none" :width "693" :height "60" :x "0" :y "220"}]
      [:use {:xlink-href "#E1-MJMATHI-3C0" :x "60" :y "676"}]
      [:use {:xlink-href "#E1-MJMAIN-34" :x "96" :y "-687"}]]]]])

(/ (sqrt 2) 2)

{:vertices ["E" "D" "F"], :sides [12 nil nil], :angles [nil (/ pi 2) (/ pi 4)]}