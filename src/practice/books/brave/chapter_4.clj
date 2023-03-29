(ns practice.books.brave.chapter-4
  (:require 
    [clojure.string :as string]))

(defn my-map [f coll]
  (seq
   (reduce (fn [acc x]
             (conj acc (f x)))
           []
           coll)))

;; (my-map inc [1 2 3]) ;; (2 3 4)
;; (= (map inc [1 2 3])
;;    (my-map inc [1 2 3])) ;; true


(defn my-filter [pred coll]
  (seq
    (reduce (fn [acc x]
              (if (pred x)
                (conj acc x)
                acc))
      []
      coll)))

;; (my-filter even? (range 10)) ;; (0 2 4 6 8)
;; (= (filter even? (range 10))
;;    (my-filter even? (range 10))) ;; true


(defn my-some [pred coll]
  (reduce (fn [_ x]
            (when (pred x)
              (reduced (pred x))))
    coll))

;; (some    even? (range 10)) ;; true
;; (my-some even? (range 10)) ;; true
;; (some    #(when (even? %) %) '(1 2 3 4)) ;; 2
;; (my-some #(when (even? %) %) '(1 2 3 4)) ;; 2


;; fwpd exercises

(def filename "resources/suspects.csv")
(def data (slurp filename))

(def vamp-keys [:name :glitter-index])

(defn str->int [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [s]
  (map #(string/split % #",")
       (string/split s #"\n")))

(defn mapify 
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
           {}
           (map vector vamp-keys unmapped-row)))
       rows))

;; (first (mapify (parse data))) ;; {:name "Edward Cullen", :glitter-index 10}

(defn glitter-filter [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

;; (glitter-filter 3 (mapify (parse data))) ;; ({:name "Edward Cullen", :glitter-index 10} {:name "Jacob Black", :glitter-index 3} {:name "Carlisle Cullen", :glitter-index 6})


;; 1

(map :name (glitter-filter 3 (mapify (parse data)))) ;; ("Edward Cullen" "Jacob Black" "Carlisle Cullen")


;; 2

(defn append [suspects new-suspect]
  (conj suspects new-suspect))

(def suspects (mapify (parse data)))

(append suspects {:name "Chase" :glitter-index 2}) ;; ({:name "Chase", :glitter-index 2} {:name "Edward Cullen", :glitter-index 10} {:name "Bella Swan", :glitter-index 0} {:name "Charlie Swan", :glitter-index 0} {:name "Jacob Black", :glitter-index 3} {:name "Carlisle Cullen", :glitter-index 6})


;; 3

(def key-validators {:name string?
                     :glitter-index integer?})

(defn validate [key-validators new-record]
  (boolean
    (and (:name new-record) 
         (:glitter-index new-record) 
         (every? true? (for [[k v] new-record
                             :when (contains? (set vamp-keys) k)
                             :let [validation-fn (k key-validators)]]
                          (validation-fn v))))))

;; (validate key-validators {:name "Chase" :glitter-index 4}) ;; true
;; (validate key-validators {:name "Chase"}) ;; false

;; 4

(defn maps->csv [maps]
  (reduce (fn [s m]
            (str s (str (:name m) "," (:glitter-index m) "\n"))) 
    ""
    maps))

;; (maps->csv suspects) ;; "Edward Cullen,10\nBella Swan,0\nCharlie Swan,0\nJacob Black,3\nCarlisle Cullen,6\n"
;; (= data (maps->csv suspects)) ;; true
