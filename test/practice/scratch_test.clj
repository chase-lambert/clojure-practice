(ns practice.scratch-test
  (:require
   [clojure.test :refer :all]
   [practice.scratch :refer :all]))

(deftest add-test
  (testing "testing add"
    (is (= 2 (+ 1 1)))))

(deftest thing
  (let [a {:a 1 :b 2 :c 3}
        b {:a 1 :b 2 :c 1}]
    (is (= a b))))
