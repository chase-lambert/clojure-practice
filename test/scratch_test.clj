(ns practice.scratch-test
  (:require 
    [clojure.test :refer :all]
    [practice.scratch :refer :all]))

(deftest a-test 
  (testing "add"
    (is (= 0 (add 1 2)))))
