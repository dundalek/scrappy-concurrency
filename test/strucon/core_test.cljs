(ns strucon.core-test
  (:require
   [clojure.test :refer [async deftest is]]
   [missionary.core :as m]
   [strucon.core :as core]))

(deftest unbounded
  (async done
         (let [!results (atom [])
               make-task  (fn [x]
                            (m/sp
                             (swap! !results conj [:begin x])
                             (m/? (m/sleep 0))
                             (swap! !results conj [:end x])))
               perform (core/unbounded)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]
                     [:begin 2]
                     [:begin 3]]
                    @!results))
             (m/? perform)
             (is (= [[:begin 1]
                     [:begin 2]
                     [:begin 3]
                     [:end 1]
                     [:end 2]
                     [:end 3]]
                    @!results))
             (done))))))

(deftest restartable-one
  (async done
         (let [!results (atom [])
               make-task  (fn [x]
                            (m/sp
                             (swap! !results conj [:begin x])
                             (m/? (m/sleep 0))
                             (swap! !results conj [:end x])))
               perform (core/restartable)]
           ((m/sp
             (perform (make-task 1))
             (is (= [[:begin 1]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]] @!results))
             (done))))))

(deftest restartable-multiple
  (async done
         (let [!results (atom [])
               make-task  (fn [x]
                            (m/sp
                             (swap! !results conj [:begin x])
                             (m/? (m/sleep 0))
                             (swap! !results conj [:end x])))
               perform (core/restartable)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1] [:begin 2] [:begin 3]]
                    @!results))
             (m/? perform)
             (is (= [[:begin 1] [:begin 2] [:begin 3] [:end 3]]
                    @!results))
             (done))))))

(deftest enqueued
  (async done
         (let [!results (atom [])
               make-task  (fn [x]
                            (m/sp
                             (swap! !results conj [:begin x])
                             (m/? (m/sleep 0))
                             (swap! !results conj [:end x])))
               perform (core/enqueued)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]]
                    @!results))
             (m/? perform)
             (is (= [[:begin 1]
                     [:end 1]
                     [:begin 2]
                     [:end 2]
                     [:begin 3]
                     [:end 3]]
                    @!results))
             (done))))))

(deftest dropping
  (async done
         (let [!results (atom [])
               make-task  (fn [x]
                            (m/sp
                             (swap! !results conj [:begin x])
                             (m/? (m/sleep 0))
                             (swap! !results conj [:end x])))
               perform (core/dropping)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]] @!results))
             (m/? (m/sleep 1))
             (perform (make-task 4))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]
                     [:begin 4] [:end 4]]
                    @!results))
             (done))))))

(deftest keeping-latest
  (async done
         (let [!results (atom [])
               make-task  (fn [x]
                            (m/sp
                             (swap! !results conj [:begin x])
                             (m/? (m/sleep 0))
                             (swap! !results conj [:end x])))
               perform (core/keeping-latest)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]] @!results))
             (m/? (m/sleep 5))
             (is (= [[:begin 1] [:end 1]
                     [:begin 3] [:end 3]]
                    @!results))
             (perform (make-task 4))
             (m/? (m/sleep 5))
             (is (= [[:begin 1] [:end 1]
                     [:begin 3] [:end 3]
                     [:begin 4] [:end 4]]
                    @!results))
             (done))))))
