(ns trig.ratio)

;; builds a map of the first 1000 divisions of pi, beginning with pi/2.
;; each decimal value is mapped to the TeX string representing its fraction.

(def pi js/Math.PI)

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(def fractions-of-pi
  (into {}
        (reverse (map (juxt (fn [n] (/ pi n))
                            (fn [n]
                              (str "\\dfrac{\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-2pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 2 pi) n))
                            (fn [n]
                              (str "\\dfrac{2\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-3pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 3 pi) n))
                            (fn [n]
                              (str "\\dfrac{3\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-4pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 4 pi) n))
                            (fn [n]
                              (str "\\dfrac{4\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-5pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 5 pi) n))
                            (fn [n]
                              (str "\\dfrac{5\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-6pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 6 pi) n))
                            (fn [n]
                              (str "\\dfrac{6\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-7pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 7 pi) n))
                            (fn [n]
                              (str "\\dfrac{7\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-8pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 8 pi) n))
                            (fn [n]
                              (str "\\dfrac{8\\pi}{" n "}")))
                      (range 2 1000)))))

(def fractions-of-9pi
  (into {}
        (reverse (map (juxt (fn [n] (/ (* 9 pi) n))
                            (fn [n]
                              (str "\\dfrac{9\\pi}{" n "}")))
                      (range 2 1000)))))

(def fracs-of-pi
  (into {}
        (map (juxt (fn [n] (/ pi n))
                   identity)
             (range 2 1000))))

(def fracs-of-2pi
  (into {}
        (map (juxt (fn [n] (/ (* 2 pi) n))
                   identity)
             (range 2 1000))))

(def fracs-of-7pi
  (into {}
        (map (juxt (fn [n] (/ (* 7 pi) n))
                   identity)
             (range 2 1000))))

(defn fractions-of [x]
  (into {}
        (map (juxt (fn [n] (/ x n))
                   (fn [n]
                     (str "\\dfrac{" x "}{" n "}")))
             (range 2 50))))

(def simple-ratios
  (into {}
        (reverse (map (fn [[n d]] [(/ n d) (str "\\dfrac{" n "}{" d "}")])
                      (for [n (range 1 100)
                            d (range 1 100)
                            :when (= 1 (gcd n d))]
                        [n d])))))

(defn ratios [end]
  (let [nums (for [n (range 1 end)
                   d (range 1 end)
                   :when (= 1 (gcd n d))]
               [n d])]
    (into {} (reverse (map (fn [[n d]] [(/ n d) [n d]])
                           nums)))))