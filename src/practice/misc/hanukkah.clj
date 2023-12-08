(ns practice.misc.hanukkah
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))



(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            (repeat))
       (rest csv-data)))


(def customer-reader 
  (io/reader "resources/5784/noahs-customers.csv"))

(def customer-data 
  (->> (csv/read-csv customer-reader)
       (csv-data->maps)))

(def orders-reader 
  (io/reader "resources/5784/noahs-orders.csv"))

(def orders-data 
  (->> (csv/read-csv orders-reader)
       (csv-data->maps)))

(def orders-item-reader 
  (io/reader "resources/5784/noahs-orders_items.csv"))

(def orders-item-data 
  (->> (csv/read-csv orders-item-reader)
       (csv-data->maps)))

(def products-reader 
  (io/reader "resources/5784/noahs-products.csv"))

(def products-data 
  (->> (csv/read-csv products-reader)
       (csv-data->maps)))


;; Day 1
(defn letter->num [letter]
  (cond 
    (#{\a \b \c} letter) 2
    (#{\d \e \f} letter) 3
    (#{\g \h \i} letter) 4
    (#{\j \k \l} letter) 5
    (#{\m \n \o} letter) 6
    (#{\p \q \r \s} letter) 7
    (#{\t \u \v} letter) 8
    (#{\w \x \y \z} letter) 9))

(defn name->number [name]
  (let [number (apply str (map letter->num (str/lower-case name)))]
    (str (subs number 0 3) "-" (subs number 3 6) "-" (subs number 6))))

(def possible-names
  (->> (map :name customer-data)
       (map #(str/split % #" "))
       (map second)
       (filter #(= 10 (count %)))
       (set)))

(def possible-numbers 
  (set (map name->number possible-names)))

(def customer-phone-numbers
  (set (map :phone customer-data)))

(comment
  ;; Day 1 solution:
  (set/intersection possible-numbers customer-phone-numbers) ;; #{"826-636-2286"}
  ,)




(comment
  ;; password to zip file was 5777)
  ,) 
