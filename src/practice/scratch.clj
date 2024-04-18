(ns practice.scratch
  (:require
   [clojure.string :as str]
   [criterium.core :as cc]))
   ;; [clojure.pprint :refer [cl-format]]
   ;; [clojure.data.json :as json]))
   ;; [clojure.core.async :as async]))
   ;; [clj-async-profiler.core :as prof]))

;; (set! *warn-on-reflection* true)

(defn sum-nums []
  (reduce (fn [acc n]
            (+ acc n))
          0
          (range 4097)))


(comment
  (cc/quick-bench sum-nums)
  (cc/quick-bench
    (reduce (fn [acc n]
             (+ acc n))
            0
            (range 4097))))
