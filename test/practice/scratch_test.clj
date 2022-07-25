(ns practice.scratch-test
  (:require 
    [clojure.test :refer :all]
    [practice.scratch :refer :all]))

(deftest add-test
  (testing "testing add"
    (is (= 2 (add 1 1)))))
