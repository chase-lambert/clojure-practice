(ns practice.core
  (:require ;; [practice.brave.peg-thing :refer [peg-thing]]))
 ;; [practice.challenges :as challenges])
   [clojure.edn :as edn]
   [practice.scratch :as scratch])
   ;; [practice.word-freq :refer :all]))
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main [& args]
  (let [arg-map (edn/read-string (first args))
        name (:name arg-map)]
    (println "Args: " arg-map "Name: " name)
    (scratch/find-person name scratch/people)))
