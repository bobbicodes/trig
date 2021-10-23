(ns trig.latex-test
  (:require [cljs.test :refer (deftest is run-tests)]
            [trig.latex :as latex]))

(deftest square-root-tex-test
  (is (= (latex/sqrt-tex 4.242640687119285)
         "3\\sqrt{2}")))

(comment
  (run-tests)
  )