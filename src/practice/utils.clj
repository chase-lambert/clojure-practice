(ns practice.utils)

(defn logging [f]
  (fn [& args]
    (prn args)
    (apply f args)))

;; (reduce (logging +) 0 [1 2 3])
;; (out) (0 1)
;; (out) (1 2)
;; (out) (3 3)
;; 6
