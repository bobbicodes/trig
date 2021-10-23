(ns trig.math)

(def pi js/Math.PI)

(defn sq [n]
  (* n n))

(defn sqrt [n]
  (.sqrt js/Math n))

(defn sin [rad]
  (.sin js/Math rad))

(defn cos [rad]
  (.cos js/Math rad))

(defn tan [rad]
  (.tan js/Math rad))

(defn asin [rad]
  (.asin js/Math rad))

(defn acos [rad]
  (.acos js/Math rad))

(defn atan [rad]
  (.atan js/Math rad))

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