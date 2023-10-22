(ns strucon.trackers-test
  (:require
   [clojure.test :refer [deftest is]]
   [strucon.trackers :as trackers]))

(deftest make-running-tracker
  (let [!state (atom 0)
        !resolve (atom nil)
        task (fn [s _f]
               (reset! !resolve s)
               (fn nop []))
        tracked-task (trackers/make-running-tracker task (partial swap! !state))]
    (is (= 0 @!state))
    (tracked-task (fn [_]) (fn [_]))
    (is (= 1 @!state))
    (@!resolve :result)
    (is (= 0 @!state))))
