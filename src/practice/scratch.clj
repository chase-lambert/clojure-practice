(ns practice.scratch 
  (:require
    [wally.main :as w]
    [wally.selectors :as ws]
    [garden.selectors :as s]))
    ;; [clojure.string :as string] 
    ;; [criterium.core :refer [bench]]
    ;; [clj-async-profiler.core :as prof]))


(w/navigate "https://clojars.org/metosin/jsonista")
(w/click [(ws/text "Copy") (ws/nth= "1")])
  
(w/fill :#search "reitit")
(w/keyboard-press "Enter")
(w/click (s/a (s/attr= :href "/metosin/reitit")))
(.textContent (w/-query (ws/text "Downloads")))
