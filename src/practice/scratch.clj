(ns practice.scratch
  (:require
   [clojure.test :refer [deftest is]]))
   ;; [clojure.pprint :refer [cl-format]]
   ;; [clojure.data.json :as json]))
   ;; [clojure.core.async :as async]))
   ;; [clj-async-profiler.core :as prof]))


(let [xf (comp (filter even?) (map inc))]
  (into [] xf [1 2 3 4 5]))

