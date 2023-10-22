(ns strucon.core-test
  #?(:cljs (:require-macros [strucon.core-test :refer [deftest+ is]]))
  (:require
   [clojure.test :as t :refer [deftest testing]]
   [missionary.core :as m]
   [strucon.core :as core]))

(def frame nil)

(defn clone-frame []
  #?(:clj (def frame (get-thread-bindings)
            #_(clojure.lang.Var/cloneThreadBindingFrame))))

(defn clear-frame []
  #?(:clj (def frame nil)))

(defn push-frame []
  #?(:clj (when frame
            (push-thread-bindings frame))))
            ; (clojure.lang.Var/resetThreadBindingFrame frame))))

(defn pop-frame []
  #?(:clj (when frame
            (pop-thread-bindings))))

(defmacro is [x]
  (if (and (sequential? x) (= 3 (count x)))
    (let [[pred expected actual] x]
      `(let [actual# ~actual]
         (push-frame)
         (try
           (t/is (~pred ~expected actual#))
           (finally
             (pop-frame)))))
    `(let [actual# ~x]
       (push-frame)
       (try
         (t/is actual#)
         (finally
           (pop-frame))))))

(defn async-task [task]
  #?(:cljs (t/async done
                    (task done done))
     :clj (m/? task)))

(defmacro deftest+ [name & body]
  `(t/deftest ~name
     (clone-frame)
     (try
       (async-task (do ~@body))
       (finally
         (clear-frame)))))

(defn- task-helper
  ([] (task-helper nil))
  ([{:keys [sleep]}]
   (let [!results (atom [])
         make-task  (fn [x]
                      (m/sp
                       (swap! !results conj [:begin x])
                       (m/? (m/sleep (or sleep x)))
                       (swap! !results conj [:end x])
                       [:result x]))]
     {:make-task make-task
      :!results !results})))

(defn wait-for-tasks [perform]
  ; (m/sleep 10)
  perform)

(deftest+ unbounded
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/unbounded)]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (is (= [[:begin 1]
             [:begin 2]
             [:begin 3]]
            @!results))
     (m/? (wait-for-tasks perform))
     (is (= [[:begin 1]
             [:begin 2]
             [:begin 3]
             [:end 1]
             [:end 2]
             [:end 3]]
            @!results)))))

(deftest+ restartable-one
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/restartable)]
    (m/sp
     (perform (make-task 1))
     (is (= [[:begin 1]] @!results))
     (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:end 1]] @!results)))))

(deftest+ restartable-multiple
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/restartable)]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (is (= [[:begin 1] [:begin 2] [:begin 3]]
            @!results))
     (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:begin 2] [:begin 3] [:end 3]]
            @!results)))))

(deftest+ restartable-max-concurrency
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/restartable {:max-concurrency 2})]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (perform (make-task 4))
     (is (= [[:begin 1] [:begin 2] [:begin 3] [:begin 4]]
            @!results))
     (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:begin 2] [:begin 3] [:begin 4] [:end 3] [:end 4]]
            @!results)))))

(deftest+ enqueued
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/enqueued)]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (is (= [[:begin 1]]
            @!results))
     ; (m/? (wait-for-tasks perform))
     (m/? (m/sleep 30))
     (is (= [[:begin 1]
             [:end 1]
             [:begin 2]
             [:end 2]
             [:begin 3]
             [:end 3]]
            @!results)))))

(deftest+ enqueued-max-concurrency
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/enqueued {:max-concurrency 2})]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (is (= [[:begin 1]
             [:begin 2]]
            @!results))
     (m/? (m/sleep 10))
     ; (m/? (wait-for-tasks perform))
     (is (= [[:begin 1]
             [:begin 2]
             [:end 1]
             ;; is begin 3 before end 3 ok or should we schedule on next tick?
             [:begin 3]
             [:end 2]
             [:end 3]]
            @!results)))))

(deftest+ dropping
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/dropping)]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (is (= [[:begin 1]] @!results))
     (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:end 1]] @!results))
     (perform (make-task 4))
     (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:end 1]
             [:begin 4] [:end 4]]
            @!results)))))

(deftest+ dropping-max-concurrency
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/dropping {:max-concurrency 2})]
    (m/sp
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
            @!results)))))

(deftest+ keeping-latest
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/keeping-latest)]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (is (= [[:begin 1]] @!results))
     ; (m/? (wait-for-tasks perform))
     (m/? (m/sleep 10))
     (is (= [[:begin 1] [:end 1]
             [:begin 3] [:end 3]]
            @!results))
     (perform (make-task 4))
     (m/? (m/sleep 10))
     ; (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:end 1]
             [:begin 3] [:end 3]
             [:begin 4] [:end 4]]
            @!results)))))

(deftest+ keeping-latest-max-concurrency
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/keeping-latest {:max-concurrency 2})]
    (m/sp
     (perform (make-task 1))
     (perform (make-task 2))
     (perform (make-task 3))
     (perform (make-task 4))
     (is (= [[:begin 1] [:begin 2]] @!results))
     (m/? (m/sleep 10))
     ; (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:begin 2]
             [:end 1] [:begin 4]
             [:end 2] [:end 4]]
            @!results))
     (perform (make-task 5))
     (m/? (m/sleep 10))
     ; (m/? (wait-for-tasks perform))
     (is (= [[:begin 1] [:begin 2]
             [:end 1] [:begin 4]
             [:end 2] [:end 4]
             [:begin 5] [:end 5]]
            @!results)))))

(deftest+ unbounded-join
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/unbounded)]
    (m/sp
     (is (= [[:result 1] [:result 2] [:result 3]]
            (m/? (m/join vector
                         (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= #{[:begin 1] [:begin 2] [:begin 3] [:end 1] [:end 2] [:end 3]}
            (set @!results))))))

(deftest+ unbounded-race
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/unbounded)]
    (m/sp
     (is (= [:result 1]
            (m/? (m/race (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= [[:begin 1] [:begin 2] [:begin 3] [:end 1]] @!results)))))

(deftest+ restartable-join
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/restartable)]
    (m/sp
     (is (= [nil nil [:result 3]]
            (m/? (m/join vector
                         (core/cancel-shield (perform (make-task 1)))
                         (core/cancel-shield (perform (make-task 2)))
                         (core/cancel-shield (perform (make-task 3)))))))
     (is (= [[:begin 1] [:begin 2] [:begin 3] [:end 3]]
            @!results)))))

(deftest+ restartable-race
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/restartable)]
    (m/sp
     (is (= [:result 3]
            (m/? (m/race (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= [[:begin 1] [:begin 2] [:begin 3] [:end 3]] @!results)))))

(deftest+ enqueued-join
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/enqueued)]
    (m/sp
     (is (= [[:result 1] [:result 2] [:result 3]]
            (m/? (m/join vector
                         (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= [[:begin 1] [:end 1] [:begin 2] [:end 2] [:begin 3] [:end 3]]
            @!results)))))

(deftest+ enqueued-race
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/enqueued)]
    (m/sp
     (is (= [:result 1]
            (m/? (m/race (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= [[:begin 1] [:end 1]] @!results)))))

(deftest+ dropping-join
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/dropping {:max-concurrency 1})]
    (m/sp
     (is (= [[:result 1] nil nil]
            (m/? (m/join vector
                         (core/cancel-shield (perform (make-task 1)))
                         (core/cancel-shield (perform (make-task 2)))
                         (core/cancel-shield (perform (make-task 3)))))))
     (is (= [[:begin 1] [:end 1]]
            @!results)))))

(deftest+ dropping-race
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/dropping {:max-concurrency 1})]
    (m/sp
     (is (= [:result 1]
            (m/? (m/race (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= [[:begin 1] [:end 1]] @!results)))))

(deftest+ keeping-latest-join
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/keeping-latest {:max-concurrency 1})]
    (m/sp
     (is (= [[:result 1] nil [:result 3]]
            (try
              (m/? (m/join vector
                           (perform (make-task 1))
                           (core/cancel-shield (perform (make-task 2)))
                           (perform (make-task 3))))
              (catch :default e
                (println "caught" e)))))
     (is (= [[:begin 1] [:end 1] [:begin 3] [:end 3]]
            @!results)))))

(deftest+ keeping-latest-race
  (let [{:keys [make-task !results]} (task-helper)
        perform (core/keeping-latest)]
    (m/sp
     (is (= [:result 1]
            (m/? (m/race (perform (make-task 1))
                         (perform (make-task 2))
                         (perform (make-task 3))))))
     (is (= [[:begin 1] [:end 1]] @!results)))))

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
          (is (core/cancelled? e))))))

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

    (testing "error gets propagated"
      ((core/cancel-shield (m/sp (throw (ex-info "oops" {}))))
       (fn [_]
         (is false))
       (fn [e]
         (swap! !assertions inc)
         (is (= "oops" (ex-message e))))))

    (is (= 4 @!assertions))))

(deftest cancelled-test
  (is (core/cancelled? (core/cancelled "foo"))))
