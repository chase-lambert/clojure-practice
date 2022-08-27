(ns practice.core
  (:require
   ;; [practice.brave.peg-thing :refer [peg-thing]]))
    [practice.challenges :as challenges])
   ;; [practice.scratch :as scratch]))
   ;; [practice.word-freq :refer :all]))
  (:gen-class))

(defn -main [& args]
  (print (challenges/format-markdown-table challenges/input-markdown))
  (flush))
