(ns practice.books.brave.chapter-3
  (:require
   [clojure.string :as s]))

;; 1

(str "Answer to the universe: " 42) ;; "Answer to the universe: 42"
(vector 1 2 3) ;; [1 2 3]
(list 1 2 3) ;; (1 2 3)
(hash-map :a 1 :b 1) ;; {:b 1, :a 1}
(hash-set :a 1 :b 1) ;; #{1 :b :a}


;; 2

(defn add-100 [n]
  (+ 100 n))

(add-100 1) ;; 101


;; 3

(defn dec-maker [n]
  (fn [num]
    (- num n)))

(def dec9 (dec-maker 9))
(dec9 10) ;; 1


;; 4

(defn mapset [f coll]
  (set (map f coll)))

(mapset inc [1 1 2 2]) ;; #{3 2}


;; 5 & 6

(def alien-body-parts
  [{:name "head" :size 3}
   {:name "eye-1" :size 1}
   {:name "ear-1" :size 1}
   {:name "mouth" :size 1}
   {:name "nose" :size 1}
   {:name "neck" :size 2}
   {:name "shoulder-1" :size 3}
   {:name "upper-arm-1" :size 3}
   {:name "chest" :size 10}
   {:name "back" :size 10}
   {:name "forearm-1" :size 3}
   {:name "abdomen" :size 6}
   {:name "kidney-1" :size 1}
   {:name "hand-1" :size 2}
   {:name "knee-1" :size 2}
   {:name "thigh" :size 4}
   {:name "lower-leg-1" :size 3}
   {:name "achilles-1" :size 1}
   {:name "foot-1" :size 2}])

(defn matching-parts [{:keys [name size]} n-parts-to-add]
  (reduce (fn [all-parts n]
            (conj all-parts {:size size
                             :name (s/replace name #"-(.*)" (str "-" n))}))
    #{}
    (range 1 (inc n-parts-to-add))))

(defn symmetrize-body-parts [alien-body-parts n-parts-to-add]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (matching-parts part n-parts-to-add)))
    []
    alien-body-parts))

(symmetrize-body-parts alien-body-parts 3)
