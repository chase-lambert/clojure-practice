(ns practice.core
  (:require
   ;; [practice.brave.peg-thing :refer [peg-thing]]))
    ;; [practice.challenges :as challenges])
   [practice.scratch :as scratch])
   ;; [practice.word-freq :refer :all]))
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main [args]
  (scratch/example))
