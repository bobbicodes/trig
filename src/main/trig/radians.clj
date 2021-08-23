(ns trig.radians
  "https://www.khanacademy.org/math/trigonometry/unit-circle-trig-func/intro-to-radians-trig/v/introduction-to-radians")

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn degrees 
  "Converts radians to degrees. 
   Numerator is considered to be a factor of pi"
  [n d]
  (* (/ 180 d) n))

(defn radians
  "Converts degrees to radians. 
   Outputs a vector of the numerator (factor of pi) and denominator."
  [deg]
  [(/ deg (gcd deg 180))
   (/ 180 (gcd deg 180))])

(comment
  (gcd 310 180)
  (degrees 14 9)
  (degrees 257 360.0)
  (degrees 5 12)
  (radians 310)
  (radians 260))