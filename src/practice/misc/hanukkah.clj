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

(filter #(str/includes? (:sku %) "BKY") orders-item-data)
;; Day 4

(let [bakery-orders (filter #(str/includes? (:sky %) "BKY") orders-item-data)
      early-orders (for [order orders-item-data
                         :let [[_ time] (str/split (:ordered order) #" ")
                               hour (parse-long (first (str/split time #":")))]
                         :when (< hour 5)]
                     order)]
  (count early-orders))




        
              
(filter (fn [order]
          (= "16504" (:orderid order)))
     orders-item-data)
        
                    
  
                            


  
         
;; Day 1
;; (defn letter->num [letter]
;;   (cond 
;;     (#{\a \b \c} letter) 2
;;     (#{\d \e \f} letter) 3
;;     (#{\g \h \i} letter) 4
;;     (#{\j \k \l} letter) 5
;;     (#{\m \n \o} letter) 6
;;     (#{\p \q \r \s} letter) 7
;;     (#{\t \u \v} letter) 8
;;     (#{\w \x \y \z} letter) 9))

;; (defn name->number [name]
;;   (let [number (apply str (map letter->num (str/lower-case name)))]
;;     (str (subs number 0 3) "-" (subs number 3 6) "-" (subs number 6))))

;; (def possible-names
;;   (->> (map :name customer-data)
;;        (map #(str/split % #" "))
;;        (map second)
;;        (filter #(= 10 (count %)))
;;        (set)))

;; (def possible-numbers 
;;   (set (map name->number possible-names)))

;; (def customer-phone-numbers
;;   (set (map :phone customer-data)))
  
;; (set/intersection possible-numbers customer-phone-numbers) ;; #{"826-636-2286"}


;; Day 2
;; (filter #(str/includes? (:desc %) "Coffee") products-data)
;; ({:sku "DLI8820",
;;   :desc "Coffee, Drip",
;;   :wholesale_cost "1.44",
;;   :dims_cm "9.6|7.8|0.7"})

(defn year-ordered [order]
  (let [ordered (:ordered order)
        date (first (str/split ordered #" "))] 
    (first (str/split date #"-"))))

(comment
  (let [possible (for [customer customer-data
                       :let [name (:name customer)
                             [first-name last-name] (str/split name #" ")
                             initials [(first first-name) (first last-name)]]
                       :when (= [\J \P] initials)]
                    customer)
        orders-in-2017 (filter #(= "2017" (year-ordered %)) orders-data)
        possible (for [customer possible
                       order orders-in-2017
                       :let [customer-id (:customerid customer)
                             order-customer-id (:customerid order)]
                       :when (= order-customer-id customer-id)]
                   [customer order])
        possible (for [[customer order] possible
                       :let [order-id (:orderid order)
                             orders (filter #(= order-id (:orderid %)) orders-item-data)]
                        :when (some #(= "DLI8820" %) (map :sku orders))]
                   customer)]
    (first possible)))
  ;; {:birthdate "1947-02-05",
  ;;  :address "100-75 148th St",
  ;;  :timezone "America/New_York",
  ;;  :long "-73.80856",
  ;;  :phone "332-274-4185",
  ;;  :name "Joshua Peterson",
  ;;  :citystatezip "Jamaica, NY 11435",
  ;;  :lat "40.70895",
  ;;  :customerid "1475"}


;; Day 3
(def rabbit-years #{2011 1999 1987 1975 1963 1951 1939 1927 1915})

(defn parse-birthdate [customer]
  (let [[year month day] (map parse-long (str/split (:birthdate customer) #"-"))]
    [year month day]))

(defn street [customer]
  (let [address (:address customer)
        [_ street _] (str/split address #" ")]
    street))

(defn city [customer]
  (let [city-state-zip (:citystatezip customer)
        [city _ _] (str/split city-state-zip #",")]
    city))

(comment
  (let [possible (filter (fn [customer]
                          (let [[year month day] (parse-birthdate customer)]
                            (and (rabbit-years year)
                                 (or 
                                   (and (= 6 month)
                                        (>= day 21))
                                   (and (= 7 month)
                                        (<= day 22))))))
                         customer-data)]
    (for [customer possible
          :when (= "Jamaica" (city customer))]
      customer)))
;; ({:birthdate "1999-07-08",
;;   :address "145-51 107th Ave",
;;   :timezone "America/New_York",
;;   :long "-73.80487",
;;   :phone "917-288-9635",
;;   :name "Robert Morton",
;;   :citystatezip "Jamaica, NY 11435",
;;   :lat "40.68959",
;;   :customerid "2550"})


(comment
  ;; password to zip file was 5777)
  ,) 
