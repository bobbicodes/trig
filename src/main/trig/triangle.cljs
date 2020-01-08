(ns trig.triangle
  (:require [reagent.core :as r]))

(defonce triangle
  (r/atom {:line1   nil
           :line2 5
           :line3 nil
           :angle1 31
           :angle2 108
           :angle3 nil
           :label1 "A"
           :label2 "B"
           :label3 "C"}))

(defn sin [deg]
  (.sin js/Math (* deg (/ js/Math.PI 180))))

(defn cos [deg]
  (.cos js/Math (* deg (/ js/Math.PI 180))))

(defn tan [deg]
  (.tan js/Math (* deg (/ js/Math.PI 180))))

(defn asin [deg]
  (* (.asin js/Math deg) (/ 180 js/Math.PI)))

(defn acos [deg]
  (* (.acos js/Math deg) (/ 180 js/Math.PI)))

(defn atan [deg]
  (* (.atan js/Math deg) (/ 180 js/Math.PI)))

(defn csc
  "Returns the cosecant, the reciprocal of the sine,
   the ratio of the hypotenuse to the side opposite 
   a given angle in a right triangle."
  [h o]
  (/ h o))

(defn sec
  "Returns the secant, the reciprocal of the cosine,
   the ratio of the hypotenuse to the side adjacent to 
   a given angle in a right triangle."
  [h a]
  (/ h a))

(defn cot
  "Returns the cotangent, the reciprocal of the tangent,
   the ratio of the adjacent side 
   to the opposite side of a right triangle."
  [a o]
  (/ a o))

(defn solve-sides [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2]} triangle]
    (cond
      (and (pos? angle1) (pos? line3))
      (assoc triangle
             :line2 (* line3 (cos angle1))
             :line1 (* line3 (sin angle1)))
      (and (pos? angle1) (pos? line2))
      (assoc triangle
             :line1 (* line2 (tan angle1))
             :line3 (/ line2 (cos angle1)))
      (and (pos? angle1) (pos? line1))
      (assoc triangle
             :line2 (/ line1 (tan angle1))
             :line3 (/ line1 (sin angle1)))
      (and (pos? (:degrees angle2)) (pos? line3))
      (assoc triangle
             :line1 (* line3 (cos angle2))
             :line2 (* line3 (sin angle2)))
      (and (pos? (:degrees angle2)) (pos? line2))
      (assoc triangle
             :line3 (/ line2 (sin angle2))
             :line1 (/ line2 (tan angle2)))
      (and (pos? angle2) (pos? line1))
      (assoc triangle
             :line2 (* line1 (tan angle2))
             :line3 (/ line1 (cos angle2)))
      :else triangle)))

(defn infer-angle [triangle]
 (let [{:keys [angle1 angle2 angle3]} triangle] 
   (cond
     (nil? angle1)
     (assoc triangle :angle1 (- 180 angle2 angle3))
     (nil? angle2)
     (assoc triangle :angle2 (- 180 angle1 angle3))
     (nil? angle3)
     (assoc triangle :angle3 (- 180 angle2 angle1)))))


(defn solve-angles [triangle]
  (let [{:keys [line1 line2 line3 angle1 angle2 angle3]} triangle]
    (cond
      (= 2 (count (filter pos? [angle1 angle2 angle3])))
      (infer-angle triangle)
      (and (pos? line1) (pos? line3))
      (-> triangle
          (assoc :angle1 (asin (/ line1 line3))
                 :angle2 (acos (/ line1 line3))))
      (and (pos? line2) (pos? line3))
      (-> triangle
          (assoc :angle1 (acos (/ line2 line3))
                 :angle2 (asin (/ line2 line3))))
      (and (pos? line2) (pos? line1))
      (-> triangle
          (assoc :angle1 (atan (/ line1 line2))
                 :angle2  (atan (/ line2 line1))))
      :else triangle)))

(defn law-of-sines [triangle]
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
      (assoc triangle :angle1 (asin (* (/ line2 line1) (sin angle3))))
      (and (pos? angle3) (pos? line1) (pos? line3))
      (assoc triangle :angle2 (asin (* (/ line3 line1) (sin angle3))))
      :else triangle)))

(defn loc-side
  "Use law of cosines to solve for a side, given the opposite angle and 2 other sides."
  [s1 s2 a]
  (.sqrt js/Math
         (- (+ (* s1 s1) (* s2 s2))
            (* 2 s1 s2 (cos a)))))

(defn loc-angle
  "Use law of cosines to solve for an angle, given an opposite and 2 adjacent sides."
  [a1 o a2]
  (acos (/ (+ (* a1 a1) (* a2 a2) (- (* o o)))
           (* 2 a1 a2))))

(defn law-of-cosines [triangle]
  (let [{:keys [angle1 angle2 angle3 line1 line2 line3]} triangle]
    (cond
      (and (pos? angle1) (pos? line1) (pos? line3))
      (assoc triangle :line2 (loc-side line3 line1 angle1))
      (and (pos? angle2) (pos? line1) (pos? line2))
      (assoc triangle :line3 (loc-side line1 line2 angle2))
      (and (pos? angle3) (pos? line2) (pos? line3))
      (assoc triangle :line1 (loc-side line2 line3 angle3))
      (and (pos? line1) (pos? line2) (pos? line3))
      (assoc triangle
             :angle1 (loc-angle line1 line2 line3)
             :angle2 (loc-angle line2 line3 line1)
             :angle3 (loc-angle line2 line1 line3))
      :else triangle)))

(comment
  (- 180 40 110)
  (let [{:keys [angle1 angle2 angle3]} @triangle]
    (= 2 (count (filter pos? [angle1 angle2 angle3]))))
  (law-of-cosines @triangle))

(defn solve-triangle [triangle]
  (-> triangle
      solve-angles
      law-of-sines
      law-of-cosines))
