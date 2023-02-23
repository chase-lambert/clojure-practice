(ns practice.scratch 
  (:require
    [clojure.string :as str]))

(defn head [[x _]]
  x)

(head [1 2 3])
(head ["a" "b" "c"])
(head [])

