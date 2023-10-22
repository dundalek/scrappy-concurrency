(ns strucon.trackers
  (:require
   [strucon.core :as core]
   [strucon.protocols :as protocols])
  #?(:clj (:import [clojure.lang IDeref IFn IRef])))

;; waiting -> running
;; waiting -> dropped
;; running -> finished
;; running -> canceled
;; running -> error
(defn- instance-state-map [state]
  {:state state
   :started? (not= state :waiting) ; rename to idle?
   :canceled? (= state :canceled)
   :error? (= state :error)
   :finished? (or (= state :canceled)
                  (= state :finished)
                  (= state :error)
                  (= state :dropped))
   :successful? (= state :finished)}) ; proably rename :finished to success

#?(:cljs
   (deftype TaskInstance [task !state]
     IFn
     (-invoke [_ failure success]
       (task failure success))

     IDeref
     (-deref [_] @!state)

     IWatchable
     (-add-watch [_ key f]
       (add-watch !state key f))
     (-remove-watch [_ key]
       (remove-watch !state key))

     protocols/Droppable
     (drop! [_] (swap! !state merge (instance-state-map :dropped)))))

#?(:clj
   (deftype TaskInstance [task !state]
     IFn
     (invoke [_ failure success]
       (task failure success))

     IDeref
     (deref [_] @!state)

     IRef
     (addWatch [_ key f]
       (add-watch !state key f))
     (removeWatch [_ key]
       (remove-watch !state key))

     protocols/Droppable
     (drop! [_] (swap! !state merge (instance-state-map :dropped)))))

(defn state-tracked-task [task !state]
  (swap! !state merge
         (instance-state-map :waiting)
         {:value nil
          :error nil})
  (fn [success failure]
    (assert (= (:state @!state) :waiting)
            "Tracked task should be started only once. Create new tracker instance for each perform")
    (swap! !state merge (instance-state-map :running))
    (let [cancel (task (fn [x]
                         (swap! !state merge
                                (instance-state-map :finished)
                                {:value x})
                         (success x))
                       (fn [e]
                         (if (core/cancelled? e)
                           (swap! !state merge
                                  (instance-state-map :canceled))
                           (swap! !state merge
                                  (instance-state-map :error)
                                  {:error e}))
                         (failure e)))]
      cancel)))

(defn time-tracked-task
  ([task !state]
   (time-tracked-task task !state
                      #?(:cljs #(js/Date.now))))
  ([task !state now]
   (swap! !state assoc
          :perform-time (now)
          :start-time nil
          :end-time nil)
   (fn [success failure]
     (swap! !state assoc :start-time (now))
     (task (fn [x]
             (swap! !state assoc :end-time (now))
             (success x))
           (fn [e]
             (swap! !state assoc :end-time (now))
             (failure e))))))

(defn tracked-task [task !state]
  (-> task
      (state-tracked-task !state)
      (time-tracked-task !state)))

(defn make-tracker [task]
  (let [!state (atom {})
        wrapped-task (tracked-task task !state)]
    (->TaskInstance wrapped-task !state)))

(defn make-running-tracker [task update-running-count!]
  (fn [s f]
    (update-running-count! inc)
    (task (fn [x]
            (update-running-count! dec)
            (s x))
          (fn [e]
            (update-running-count! dec)
            (f e)))))
