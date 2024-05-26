(ns practice.scratch
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [criterium.core :as cc]))
   ;; [clojure.pprint :refer [cl-format]]
   ;; [clojure.data.json :as json]))
   ;; [clojure.core.async :as async]))
   ;; [clj-async-profiler.core :as prof]))

(set! *warn-on-reflection* true)

(defn sum ^long [^long depth ^long x]
  (if (zero? depth) 
    x
    (let [fst (sum (dec depth) (* x 2))
          snd (sum (dec depth) (inc (* x 2)))]
      (+ fst snd))))

(def memoized-sum (memoize sum))

(defn optimized-sum 
  ^long [^long depth ^long x]
 (if (zero? depth)
   x
   (let [fst (memoized-sum (dec depth) (* x 2))
         snd (memoized-sum (dec depth) (inc (* x 2)))]
     (+ fst snd))))

(comment
  (cc/quick-bench (sum 30 0))
  (cc/quick-bench (optimized-sum 30 0))
 ,)

  
  
