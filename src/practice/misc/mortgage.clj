(ns practice.misc.mortgage
  (:require [clojure.string :as str]))

(defn read-double [prompt]
  (print prompt)
  (flush)
  (let [input   (read-line)
        trimmed (str/trim input)]
    (try
      (Double/parseDouble trimmed)
      (catch NumberFormatException _e
        (println "Please enter a valid number")
        (read-double prompt)))))

(defn calculate-mortgage []
  (let [principal    (read-double "Principal: ")
        rate         (read-double "Annual Interest Rate: ")
        period       (read-double "Period (Years): ")
        monthly-rate (/ (/ rate 100) 12)
        payments     (* period 12)
        pow-factor   (Math/pow (inc monthly-rate) payments)
        calculation  (/ (* principal (* monthly-rate pow-factor))
                        (- pow-factor 1))]
    (println (format "Mortgage Payment: $%.2f" calculation))))

