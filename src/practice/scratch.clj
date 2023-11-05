(ns practice.scratch
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.pprint :refer [cl-format]]))
   ;; [clojure.core.async :as async]))
   ;; [clj-async-profiler.core :as prof]))


(defn create-user [name age email state]
  {:name  name
   :age   age
   :email email
   :state state})

(defn describe [{:keys [name age]}]
  (str name " is " age " years old"))

(def me (create-user "Chase" 43 "chase@foo.com" "NC"))

(describe me)
