(ns practice.misc.area
  (:require 
    ;; [clj-async-profiler.core :as prof]
    [clojure.math            :as math]
    [criterium.core          :refer [bench]]))

;; (set! *unchecked-math* true)

;; (defmulti area :shape)

;; (defmethod area :square [{:keys [side]}]
;;   (* side side))

;; (defmethod area :rectangle [{:keys [width height]}]
;;   (* width height))

;; (defmethod area :triangle [{:keys [base height]}]
;;   (* 0.5 base height))

;; (defmethod area :circle [{:keys [radius]}]
;;   (* math/PI radius radius))

;; (defmethod area :square [shape]
;;   (* ^double (:side shape)
;;      ^double (:side shape)))
     

;; (defmethod area :rectangle [shape]
;;   (* ^double (:width  shape)
;;      ^double (:height shape)))

;; (defmethod area :triangle [shape]
;;   (* 0.5 
;;      ^double (:base   shape)
;;      ^double (:height shape)))

;; (defmethod area :circle [shape]
;;   (* math/PI
;;      ^double (:radius shape)
;;      ^double (:radius shape)))


;; (defn make-random-shape []
;;   (let [shapes   [:square :rectangle :circle :triangle]
;;         shape    (rand-nth shapes)
;;         lengths (case shape 
;;                    :square    {:side   (double (rand-int 10))}
;;                    :rectangle {:width  (double (rand-int 10))
;;                                :height (double (rand-int 10))}
;;                    :triangle  {:base   (double (rand-int 10))
;;                                :height (double (rand-int 10))}
;;                    :circle    {:radius (double (rand-int 10))})]
;;     (merge {:shape shape} lengths)))


;; (defn total-area [shapes]
;;   (transduce (map area) + 0.0 shapes))

;; (def lots-of-shapes (doall (repeatedly 1000 #(make-random-shape))))
;; (crit/bench (total-area lots-of-shapes))  ;; 66 ms which is 66,000 microseconds
;; (prof/profile (total-area lots-of-shapes))
;; (prof/serve-ui 8080)


(defn def-shape [registry type-kw area-fn]
  (assoc registry type-kw {:storage [] :area area-fn}))

(defn add-shape [registry type-kw shape]
  (update-in registry [type-kw :storage] conj shape))

(defn shape-area [{:keys [storage area]}]
  (transduce (map area) + 0.0 storage))

(defn total-area [registry]
  (let [shapes (vals registry)]
    (reduce (fn [sum shape] (+ sum (shape-area shape))) 0 shapes)))


(deftype Circle [^double radius])
(defn circle-area [^Circle circle]
  (* math/PI ^double (.radius circle) ^double (.radius circle)))

(deftype Square [^double side])
(defn square-area [^Square square]
  (* ^double (.side square) ^double (.side square)))

(deftype Rectangle [^double width ^double height])
(defn rectangle-area [^Rectangle rectangle]
  (* ^double (.width rectangle) ^double (.height rectangle)))

(deftype Triangle [^double base ^double height])
(defn triangle-area [^Triangle triangle]
  (* ^double (.base triangle) ^double (.height triangle)))

(def shape-registry
  (let [registry (-> {}
                     (def-shape :circle circle-area)
                     (def-shape :square square-area)
                     (def-shape :rectangle rectangle-area)
                     (def-shape :triangle triangle-area))]
    (reduce (fn [registry _]
              (case (rand-nth [:circle :square :rectangle :triangle])
                :circle (-> registry (add-shape :circle (Circle. (* 42 (rand)))))
                :square (-> registry (add-shape :square (Square. (* 42 (rand)))))
                :rectangle (-> registry (add-shape :rectangle (Rectangle. (* 42 (rand)) (* 42 (rand)))))
                :triangle (-> registry (add-shape :triangle (Triangle. (* 42 (rand)) (* 42 (rand)))))))
      registry
      (range 400000))))

(bench
 (total-area shape-registry))
