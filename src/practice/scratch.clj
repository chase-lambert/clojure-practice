(ns practice.scratch
  (:require
   [clojure.string :as str]
   [criterium.core :as cc]))
   ;; [clojure.pprint :refer [cl-format]]
   ;; [clojure.data.json :as json]))
   ;; [clojure.core.async :as async]))
   ;; [clj-async-profiler.core :as prof]))

(set! *warn-on-reflection* true)

;; (def village
;;   [{:home :north :family "smith" :name "sue" :age 37 :sex :f :role :parent}
;;    {:home :north :family "smith" :name "stan" :age 35 :sex :m :role :parent}
;;    {:home :north :family "smith" :name "simon" :age 7 :sex :m :role :child}
;;    {:home :north :family "smith" :name "sadie" :age 5 :sex :f :role :child}

;;    {:home :south :family "jones" :name "jill" :age 45 :sex :f :role :parent}
;;    {:home :south :family "jones" :name "jeff" :age 45 :sex :m :role :parent}
;;    {:home :south :family "jones" :name "jackie" :age 19 :sex :f :role :child}
;;    {:home :south :family "jones" :name "jason" :age 16 :sex :f :role :child}
;;    {:home :south :family "jones" :name "june" :age 14 :sex :f :role :child}

;;    {:home :west :family "brown" :name "billie" :age 55 :sex :f :role :parent}
;;    {:home :west :family "brown" :name "brian" :age 23 :sex :m :role :child}
;;    {:home :west :family "brown" :name "bettie" :age 29 :sex :f :role :child}

;;    {:home :east :family "williams" :name "walter" :age 23 :sex :m :role :parent}
;;    {:home :east :family "williams" :name "wanda" :age 3 :sex :f :role :child}])


(defn random-home []
  (let [locations [:north :south :east :west]
        families ["smith" "jones" "brown" "williams"]
        sex-options [:m :f]
        location (rand-nth locations)
        family (rand-nth families)
        name (str/join (repeatedly 5 #(char (+ 97 (rand-int 26))))) ; Generate a random 5-letter name
        age (+ 1 (rand-int 100)) ; Age between 1 and 100
        sex (rand-nth sex-options)
        role (if (< age 18) :child :parent)]
    {:home location
     :family family
     :name name
     :age age
     :sex sex
     :role role}))

(defn generate-village [num-homes]
  (repeatedly num-homes random-home))

(defn child? [home]
  (= :child (:role home)))

(defn num-children [village]
  (count (filter child? village)))

(defn num-children-transduce [village]
  (transduce (filter child?)
             (fn
               ([acc] acc)             ; Completion arity (optional in this case)
               ([acc _] (inc acc)))    ; Step arity: increment accumulator for each item
             0                         ; Initial accumulator value
             village))

(comment 
  (def village (generate-village 100000))
  (take 2 village)
  (num-children village)
  (cc/quick-bench (num-children village))
  (cc/quick-bench (count (filterv child? village)))
  (cc/bench (num-children village))
  (num-children-transduce village)
  (cc/quick-bench (num-children-transduce village))
  (cc/bench (num-children-transduce village)))


(defn rand-location []
  (rand-nth [:north :south :east :west]))

(defn rand-family []
  (rand-nth ["smith" "jones" "brown" "williams"]))

(defn rand-name []
  (str/join (repeatedly 5 #(char (+ 97 (rand-int 26))))))

(defn rand-age []
  (+ 1 (rand-int 100)))

(defn rand-sex []
  (rand-nth [:m :f]))

(defn rand-role [age]
  (if (< age 18) :child :parent))

(defn generate-village-soa [num-homes]
  (let [ages (repeatedly num-homes rand-age)]
    {:locations (repeatedly num-homes rand-location)
     :families (repeatedly num-homes rand-family)
     :names (repeatedly num-homes rand-name)
     :ages ages
     :sexes (repeatedly num-homes rand-sex)
     :roles (map rand-role ages)}))

(defn child-soa? [role]
  (= :child role))

(defn num-children-soa [soa-village]
  (count (filter child-soa? (:roles soa-village))))

(defn num-children-soa-transduce [soa-village]
  (transduce (filter child-soa?)
             (fn
               ([acc] acc)             ; Completion arity (optional in this case)
               ([acc _] (inc acc)))    ; Step arity: increment accumulator for each item
             0                         ; Initial accumulator value
             (:roles soa-village)))

(comment 
  (def soa-village (generate-village-soa 100000))
  (:roles soa-village)
  (cc/quick-bench (num-children-soa soa-village))
  (cc/bench (num-children-soa soa-village))
  (cc/quick-bench (num-children-soa-transduce soa-village))
  (cc/bench (num-children-soa-transduce soa-village))
  ,)
