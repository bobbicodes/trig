(ns trig.uc
  (:require [trig.latex :as latex :refer [render-letter letters]]))

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

(defn axes []
  [:g
  [:path {:fill "none" :stroke "#ffcc00" :d "M 200,225 S 200,225 1.05,225" :stroke-width "2" :opacity 1}]
[:path {:fill "none" :stroke "#ffcc00" :d "M 200,225 S 200,225 398.95,225" :stroke-width "2" :opacity 1}]
[:path {:fill "none" :stroke "#ffcc00" :d "M 200,225 S 200,225 200,423.95" :stroke-width "2" :opacity 1}]
[:path {:fill "none" :stroke "#ffcc00" :d "M 200,225 S 200,225 200,26.05" :stroke-width "2" :opacity 1}]])

(defn ticks []
  [:g
   [:path {:fill "none" :stroke "#ffcc00" :d "M 283.333,230 L 283.333,220" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 116.667,230 L 116.667,220" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 195,141.667 L 205,141.667" :stroke-width 1 :opacity 1}]
   [:path {:fill "none" :stroke "#ffcc00" :d "M 195,308.333 L 205,308.333" :stroke-width 1 :opacity 1}]])

(defn radius []
 [:path {:fill "none" :stroke "#ffcc00" :d "M 200,225 L 82.1489,342.851"  :stroke-width "3" :clip-path "url(#clip-1)"}])

(defn circumference []
 [:ellipse {:cx 200 :cy 225 :rx 166.66666666666669 :ry 166.66666666666669 :fill "#29abca" :stroke "#29abca" :stroke-width "3" :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "0" :fill-opacity "0"}])

(defn side []
  [:path {:fill "none" :stroke "#ffcc00" :d "M 82.1489,225 L 82.1489,342.851"  :stroke-width 1 :clip-path "url(#clip-1)" :stroke-dasharray "4,3"}])

(defn point []
 [:ellipse {:cx "82.14886980224206" :cy "342.851130197758" :rx 5 :ry 5 :fill "#e1a158" :stroke "#e1a158"  :stroke-width "3" :clip-path "url(#clip-1)"}])

(defn arcs []
  [:g [:path {:fill "#ffcc00" :stroke "#a24d61" :d "M 141.667,225 A 58.3333,58.3333 0 0,0 158.752,266.248" :stroke-width "2" :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "0" :fill-opacity "0"}]
   [:path {:fill "#ffcc00" :stroke "#a24d61" :d "M 138.333,225 A 61.6667,61.6667 0 0,0 156.395,268.605" :stroke-width "2" :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "0" :fill-opacity "0"}]])

(defn arc []
  [:path {:fill "#ffcc00" :stroke "#ffcc00" :d "M 266.667,225 A 66.6667,66.6667 0 1,0 152.86,272.14" :stroke-width "2" :clip-path "url(#clip-1)" :stroke-opacity 1 :stroke-dasharray "8,6" :fill-opacity "0"}])

(def uc
  (fn []
    [:svg {:width 425 :height 425 :view-box "0 0 425 425"}
     [:defs [:clip-path {:id "clip-1"} [:rect {:x 0 :y 25 :width 400 :height 400}]]]
     [grid]
     [arrows]
     [axes]
     [ticks]
     [circumference]
     [radius]
     [side]
     [point]
     [arcs]
     [arc]]))

(def pi-over-6 "M17 6h23M15 7h25M14 8h26M13 9h26M12 10h4M21 10h3M29 10h2M11 11h3M21 11h3M29 11h2M11 12h2M21 12h2M29 12h2M10 13h2M20 13h3M28 13h3M20 14h3M28 14h3M20 15h3M28 15h3M19 16h3M28 16h3M19 17h3M28 17h3M19 18h3M27 18h4M18 19h3M27 19h4M18 20h3M27 20h4M17 21h4M27 21h4M17 22h4M27 22h4M16 23h4M27 23h4M16 24h4M27 24h5M16 25h4M27 25h5M15 26h5M27 26h5M15 27h4M28 27h5M15 28h4M28 28h4M15 29h3M29 29h3M6 51h38M6 52h38M6 53h38M23 68h9M21 69h12M20 70h5M31 70h3M19 71h4M30 71h4M18 72h4M29 72h6M17 73h4M29 73h6M17 74h4M29 74h6M16 75h4M29 75h5M15 76h5M30 76h3M15 77h5M15 78h5M14 79h6M14 80h5M24 80h4M14 81h5M22 81h8M14 82h5M21 82h3M27 82h5M14 83h8M28 83h5M14 84h7M29 84h5M13 85h8M30 85h4M13 86h7M30 86h5M13 87h7M30 87h5M13 88h7M30 88h6M13 89h6M30 89h6M14 90h5M30 90h6M14 91h5M30 91h6M14 92h5M30 92h6M14 93h5M30 93h6M14 94h5M30 94h6M14 95h5M30 95h6M15 96h4M30 96h6M15 97h5M30 97h5M15 98h5M30 98h5M16 99h4M30 99h4M17 100h4M29 100h5M17 101h5M28 101h5M18 102h5M27 102h5M20 103h11M21 104h8")

(def uc-1
  (fn []
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
        [:use {:xlink-href "#E1-MJMAIN-36" :x "96" :y "-687"}]]]]]))
