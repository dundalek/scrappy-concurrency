(ns strucon.trackers-test
  (:require
   [clojure.test :refer [deftest is]]
   [strucon.trackers :as trackers]))

(deftest make-running-tracker
  (let [!state (atom 0)
        !resolve (atom [])
        !reject (atom [])
        task (fn [s f]
               (swap! !resolve conj s)
               (swap! !reject conj f)
               (fn nop []))
        tracked-task (trackers/make-running-tracker task (partial swap! !state))]
    (is (= 0 @!state))
    (tracked-task (fn [result] (is (= :foo result)))
                  (fn [_] (is false "First task should not get rejected")))
    (is (= 1 @!state))
    (tracked-task (fn [_] (is false "Second task should not get resolved"))
                  (fn [e] (is (= :err e))))
    (is (= 2 @!state))
    ((first @!resolve) :foo)
    (is (= 1 @!state))
    ((second @!reject) :err)
    (is (= 0 @!state))))
