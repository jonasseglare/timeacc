(ns timeacc.core-test
  (:require [clojure.test :refer [deftest is]]
            [timeacc.core :as timeacc]))

(def the-root (timeacc/root))

(timeacc/def-unsafe-acc my-inc-acc the-root)

(defn my-inc [x]
  (timeacc/measure my-inc-acc
    (+ x 1)))

(deftest my-inc-test
  (timeacc/reset the-root)
  (is (= 4 (my-inc 3)))
  (is (= 3 (my-inc 2)))
  (is (= 41 (my-inc 40)))
  (let [m (timeacc/acc-map the-root)]
    (is (= [:my-inc-acc] (keys m)))
    (is (= 3 (timeacc/counter (:my-inc-acc m))))))
