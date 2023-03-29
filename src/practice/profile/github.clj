(ns profile.github
  (:require
    [profile.app-materials    :refer [cover-letter resume clojure-or-rust-job-offers]]
    [profile.side-effects     :refer [send-email update-cover-letter]]
    [profile.accomplish-goals :refer [crush-interview! accept-offer!]]))

(def about-me
  {:name  "Chase Lambert"
   :email "chaselambert@gmail.com"
   :desc  "I am a self taught developer who has experience building full stack web apps.
           I am looking for a new role using Clojure/ClojureScript or Rust.
           I prefer remote but will consider relocation within the US.
           I have mainly focused on web development but am open to other projects as well"})

(def profile-links
  {:github   "https://github.com/chase-lambert"
   :linkedin "https://www.linkedin.com/in/chase-lambert/"})

(def projects
  {:lesson-planner  {:url  "https://lessonplanner.onrender.com/"
                     :repo "https://github.com/chase-lambert/lesson-planner"
                     :desc "Full stack Clojure/ClojureScript app that helps teachers 
                            build lesson plans and materials using OpenAI technologies."}

   :distance-finder {:url  "https://distancefinder.onrender.com/"
                     :repo "https://github.com/chase-lambert/distancefinder"
                     :desc "Final project for Harvard's CS50 course 
                            Built using Python with Flask"}})

(def profile (merge about-me profile-links projects))

(defn send-job-application [profile cover-letter resume job-offer]
  (let [app     (assoc profile :cover-letter cover-letter :resume resume)
        contact (:email job-offer)]
    (send-email app contact)))

(defn find-new-job [job-offers]
  (doseq [{:keys [desc location offer-package] :as offer} job-offers
          :when (and (= desc "interesting challenge")
                     (= location :remote-or-enticing-offer-to-relocate)
                     (= offer-package :exciting))
          :let [letter (update-cover-letter cover-letter)]]
    (send-job-application profile letter resume offer)
    (crush-interview!)
    (accept-offer!)))

(comment 
  (find-new-job clojure-or-rust-job-offers))
