(ns practice.challenges
  (:require
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


;; rendezvous with cassidoo challenge: 22-09-04
(defn from-to [lower upper]                                                                      
  (let [gen (atom (dec lower))]                                                                  
    (fn []                                                                                       
      (when (< @gen upper)                                                                       
        (swap! gen inc)                                                                          
        (deref gen)))))                                                                          
                                                                                                 
(deftest from-to-test                                                                            
  (let [gen (from-to 5 7)]                                                                       
    (is (= 5 (gen)))                                                                             
    (is (= 6 (gen)))                                                                             
    (is (= 7 (gen)))                                                                             
    (is (= nil (gen)))))


;; rendezvous with cassidoo: 22-09-18
(def grade->points                                                                               
  {:A  4                                                                                         
   :A- 3.7                                                                                       
   :B+ 3.3                                                                                       
   :B  3                                                                                         
   :B- 2.7                                                                                       
   :C+ 2.3                                                                                       
   :C  2                                                                                         
   :C- 1.7                                                                                       
   :D+ 1.3                                                                                       
   :D  1                                                                                         
   :D- 0.7                                                                                       
   :F  0})                                                                                       
                                                                                                 
(defn calculate-gpa [grades]                                                                     
  (let [raw-gpa (/ (reduce + (map grade->points grades))                                         
                   (count grades))]                                                              
    (if (double? raw-gpa)                                                                        
      (parse-double (format "%.1f" raw-gpa))                                                     
      raw-gpa)))                                                                                 
                                                                                                 
(deftest calculate-gpa-test                                                                      
  (is (= 4   (calculate-gpa [:A])))                                                              
  (is (= 0   (calculate-gpa [:F :F :F])))                                                        
  (is (= 3.3 (calculate-gpa [:A :A- :B+ :B :B-])))                                               
  (is (= 3.3 (calculate-gpa [:A :B+ :C- :A]))))


;; rendezvous with cassidoo challenge: 22-10-02
(defn fibber [a b]                                                                               
  (lazy-seq                                                                                      
    (cons a (fibber b (+ a b)))))                                                                
                                                                                                 
(defn fib-like [a b n]                                                                           
  (take n                                                                                        
        (fibber a b)))                                                    
                                                                                                 
(deftest fib-like-test                                                                           
  (let [n 5]                                                                                     
    (is (= [10 20 30 50 80] (fib-like 10 20 n)))                                                 
    (is (= [ 3  7 10 17 27] (fib-like 3 7 n)))))


;; rendezvous with cassidoo challenge: 22-10-10
(defn truncate [s n]                                                                             
  (let [tokens (re-seq #"\W+|_|[a-zA-Z]+" s)                                                     
        word?  #(re-find #"\w+" %)                                                               
        truncate-w (fn [token]                                                                   
                     (if (and (word? token)                                                      
                              (> (count token) n))                                               
                       (subs token 0 n)                                                          
                       token))                                                                   
        truncated (map truncate-w tokens)]                                                       
    (apply str truncated)))
                                                                                                 
(deftest truncate-test                                                                           
  (let [n 3]                                                                                     
    (is (= "nev gon giv you up" (truncate "never gonna give you up" n)))                         
    (is (= "*hel* dar, my ~old_fri" (truncate "*hello* darkness, my ~old_friend" n)))))


;; rendezvous with cassidoo challenge: 22-10-17
(defn pass-doors [n number-of-passes]                                                            
  (let [initial (repeat n 1)                                                                     
        passes  (range number-of-passes)                                                         
        toggle-door     (fn [d] (if (zero? d) 1 0))                                                 
        create-sections (fn [doors pass]                                                          
                          (map vec (partition-all (inc pass) doors)))                             
        toggle-section  (fn [section pass]                                                        
                          (if (< pass (count section))                                            
                            (assoc section pass (toggle-door (last section)))                     
                            section))                                                             
        run-passes (reduce (fn [doors pass]                                                      
                             (mapcat #(toggle-section % pass) (create-sections doors pass)))      
                           initial                                                               
                           passes)]                                                              
    (count                                                                                       
      (filter zero? run-passes))))                                                               
                                                                                                 
(deftest pass-doors-test                                                                         
  (let [n 7                                                                                      
        number-of-passes 3]                                                                      
    (is (= 4 (pass-doors n number-of-passes)))))


;; rendezvous with cassidoo challenge: 22-10-30
(defn print-ascii []                                                                         
  (doseq [i (range 0x20 0x7F)]                                                                      
    (print (char i))))
    
;; If you want to print in sections exactly as shown in the prompt
(defn print-ascii-sections []                                                                    
  (doseq [section (partition-all 16 (range 0x20 0x7F))]                                                   
    (println (apply str (map char section)))))


;; rendezvous with cassidoo challenge: 23-01-29
(defn generate-arrays [n]
  (mapv #(vec (range 1 (inc %))) (range 1 (inc n))))

(deftest generate-arrays-test 
  (is (= (generate-arrays 4) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (generate-arrays 1) [[1]])))


;; rendezvous with cassidoo challenge: 23-01-29
(defn paren-as-num [c]
  (case c 
    \(  1
    \) -1
    0))

(defn num-balanced [s-of-parens]
  (->> s-of-parens
       (map paren-as-num)
       (reduce +)
       abs))

(deftest num-balanced-test
  (is (= (num-balanced "()") 0))
  (is (= (num-balanced "(()") 1))
  (is (= (num-balanced "))()))))()") 6))
  (is (= (num-balanced ")))))") 5)))



