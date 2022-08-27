(ns practice.challenges
  (:require
   [clojure.set    :as set]
   [clojure.string :as s]
   [clojure.test   :refer [deftest is]]))

;; rendezvous with cassidoo: 22-07-31
;; Number of Ones 

(defn n->digits 
  "ex. (n->digits 14) => [1 4]"
  [n]
  (if (< n 10)
    [n]
    (conj (n->digits (quot n 10)) (rem n 10))))

(defn number-of-ones [n]
  (->> (range (inc n))
       (mapcat n->digits)
       (filter #(= 1 %))
       (count)))

;; (number-of-ones 14) ;; 7


;; rendezvous with cassidoo: 22-08-08
;; Swap Pairs

(defn swap-pairs [coll]
  (->> (partition 2 coll)
       (map (fn [[a b]] 
              [b a]))
       (flatten)
       (into [])))
     
;; (swap-pairs '(1 2 3 4)) ;; [2 1 4 3]

  
;; rendezvous with cassidoo: 22-08-21
;; Format Markdown Table

(defn format-column [column width]
  (for [row column
        :let [word     (s/trim row)
              new-word (str "| " (format (str "%-" (dec width) "s") word))
              dashes   (str "| " (apply str (repeat (- width 2) "-")) " ")]]
    (if (= (second word) \-)
      dashes
      new-word)))

(defn format-markdown-table [markdown-string]
  (let [rows        (s/split-lines markdown-string)
        split-rows  (map rest 
                         (map #(s/split % #"\|") rows))
        columns     (apply map vector split-rows)
        new-columns (for [column columns
                          :let [width (apply max
                                             (map count column))]]
                      (format-column column width))
        new-rows    (apply map vector new-columns)]
    (reduce (fn [s row]
              (str s (apply str row) "|\n"))
            ""
            new-rows)))

(def input-markdown
  "| Syntax | Description |
   | --- | ----------- |
   | Header | Title |
   | Paragraph | Text |")

;; (format-markdown-table input-markdown)
;;   "| Syntax    | Description |
;;    | --------- | ----------- |
;;    | Header    | Title       |
;;    | Paragraph | Text        |"

