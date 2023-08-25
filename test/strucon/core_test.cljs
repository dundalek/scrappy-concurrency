(ns strucon.core-test
  (:require
   [clojure.test :refer [async deftest is testing]]
   [missionary.core :as m]
   [strucon.core :as core])
  (:import
   (missionary Cancelled)))

(defn- task-helper
  ([] (task-helper nil))
  ([{:keys [sleep] :or {sleep 0}}]
   (let [!results (atom [])
         make-task  (fn [x]
                      (m/sp
                       (swap! !results conj [:begin x])
                       (m/? (m/sleep sleep))
                       (swap! !results conj [:end x])
                       [:result x]))]
     {:make-task make-task
      :!results !results})))

(deftest unbounded
  (async done
         (let [{:keys [make-task !results]} (task-helper)
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
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/restartable)]
           ((m/sp
             (perform (make-task 1))
             (is (= [[:begin 1]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]] @!results))
             (done))))))

(deftest restartable-multiple
  (async done
         (let [{:keys [make-task !results]} (task-helper)
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

(deftest restartable-max-concurrency
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/restartable {:max-concurrency 2})]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (perform (make-task 4))
             (is (= [[:begin 1] [:begin 2] [:begin 3] [:begin 4]]
                    @!results))
             (m/? perform)
             (is (= [[:begin 1] [:begin 2] [:begin 3] [:begin 4] [:end 3] [:end 4]]
                    @!results))
             (done))))))

(deftest enqueued
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/enqueued)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]]
                    @!results))
             ; (m/? perform)
             (m/? (m/sleep 30))
             (is (= [[:begin 1]
                     [:end 1]
                     [:begin 2]
                     [:end 2]
                     [:begin 3]
                     [:end 3]]
                    @!results))
             (done))))))

(deftest enqueued-max-concurrency
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/enqueued {:max-concurrency 2})]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]
                     [:begin 2]]
                    @!results))
             (m/? (m/sleep 10))
             ; (m/? perform)
             (is (= [[:begin 1]
                     [:begin 2]
                     [:end 1]
                     ;; is begin 3 before end 3 ok or should we schedule on next tick?
                     [:begin 3]
                     [:end 2]
                     [:end 3]]
                    @!results))
             (done))))))

(deftest dropping
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/dropping)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]] @!results))
             (perform (make-task 4))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]
                     [:begin 4] [:end 4]]
                    @!results))
             (done))))))

(deftest dropping-max-concurrency
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/dropping {:max-concurrency 2})]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (perform (make-task 4))
             (is (= [[:begin 1] [:begin 2]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:begin 2] [:end 1] [:end 2]] @!results))
             (perform (make-task 5))
             (m/? perform)
             (is (= [[:begin 1] [:begin 2]
                     [:end 1] [:end 2]
                     [:begin 5] [:end 5]]
                    @!results))
             (done))))))

(deftest keeping-latest
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/keeping-latest)]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (is (= [[:begin 1]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]
                     [:begin 3] [:end 3]]
                    @!results))
             (perform (make-task 4))
             (m/? perform)
             (is (= [[:begin 1] [:end 1]
                     [:begin 3] [:end 3]
                     [:begin 4] [:end 4]]
                    @!results))
             (done))))))

(deftest keeping-latest-max-concurrency
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/keeping-latest {:max-concurrency 2})]
           ((m/sp
             (perform (make-task 1))
             (perform (make-task 2))
             (perform (make-task 3))
             (perform (make-task 4))
             (is (= [[:begin 1] [:begin 2]] @!results))
             (m/? perform)
             (is (= [[:begin 1] [:begin 2]
                     [:end 1] [:begin 4]
                     [:end 2] [:end 4]]
                    @!results))
             (perform (make-task 5))
             (m/? perform)
             (is (= [[:begin 1] [:begin 2]
                     [:end 1] [:begin 4]
                     [:end 2] [:end 4]
                     [:begin 5] [:end 5]]
                    @!results))
             (done))))))

(deftest enqueued-join
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/enqueued {:max-concurrency 1})]
           ((m/sp
             (is (= [[:result 1] [:result 2] [:result 3]]
                    (m/? (m/join vector
                                 (perform (make-task 1))
                                 (perform (make-task 2))
                                 (perform (make-task 3))))))
             (is (= [[:begin 1] [:end 1] [:begin 2] [:end 2] [:begin 3] [:end 3]]
                    @!results))
             (done))))))

(deftest enqueued-race
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/enqueued {:max-concurrency 1})]
           ((m/sp
             (is (= [:result 1]
                    (m/? (m/race (perform (make-task 1))
                                 (perform (make-task 2))
                                 (perform (make-task 3))))))
             (is (= [[:begin 1] [:end 1]] @!results))
             (done))))))

(deftest keeping-latest-join
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/keeping-latest {:max-concurrency 1})]
           ((m/sp
             (is (= [[:result 1] nil [:result 3]]
                    (try
                      (m/? (m/join vector
                                   (perform (make-task 1))
                                   (core/cancel-shield (perform (make-task 2)))
                                   (perform (make-task 3))))
                      (catch :default e
                        (js/console.log "caught" e)))))
             (is (= [[:begin 1] [:end 1] [:begin 3] [:end 3]]
                    @!results))
             (done))))))

(deftest keeping-latest-race
  (async done
         (let [{:keys [make-task !results]} (task-helper)
               perform (core/keeping-latest {:max-concurrency 1})]
           ((m/sp
             (is (= [:result 1]
                    (m/? (m/race (perform (make-task 1))
                                 (perform (make-task 2))
                                 (perform (make-task 3))))))
             (is (= [[:begin 1] [:end 1]] @!results))
             (done))))))

(deftest cancel-shield
  (let [!assertions (atom 0)
        task (m/sp
              (m/? m/never))]

    (testing "cancellation is treated as failure"
      ((task
        (fn []
          (is false))
        (fn [e]
          (swap! !assertions inc)
          (is (instance? Cancelled e))))))

    (testing "cancellation wrapper makes it succeed"
      (((core/cancel-shield task)
        (fn [x]
          (swap! !assertions inc)
          (is (= nil x)))
        (fn [_e]
          (is false)))))

    (testing "success returns result"
      ((core/cancel-shield (m/sp :result))
       (fn [x]
         (swap! !assertions inc)
         (is (= :result x)))
       (fn [_e]
         (is false))))

    (is (= 3 @!assertions))))
