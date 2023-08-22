(ns strucon.app
  (:require
   ["react-dom" :as rdom]
   [goog.string :as gstr]
   [helix.core :refer [$ <>]]
   [helix.dom :as d]
   [helix.hooks :as hooks]
   [missionary.core :as m]
   [strucon.core :as core]
   [strucon.lib :refer [defnc]]))

(def track-height 20)
(def colors ["red", "green", "blue"])

(defn start-animation [f]
  (let [!request (atom nil)]
    (letfn [(animate [t]
              (f t)
              (reset! !request (js/requestAnimationFrame animate)))]
      (reset! !request (js/requestAnimationFrame animate))
      #(js/cancelAnimationFrame @!request))))

(defn use-animation []
  (let [[time set-time!] (hooks/use-state nil)
        [start-time set-start-time!] (hooks/use-state nil)
        !stop-animation (hooks/use-ref nil)
        start (hooks/use-callback*
               (fn []
                 (when-not @!stop-animation
                   (let [t (js/Date.now)]
                     (reset! !stop-animation (start-animation #(set-time! (js/Date.now))))
                     (set-start-time! t)
                     (set-time! t)))))
        stop (hooks/use-callback*
              (fn []
                (when-some [stop @!stop-animation]
                  (stop)
                  (reset! !stop-animation nil))
                (set-start-time! nil)
                (set-time! nil)))]
    {:start-time start-time
     :time time
     :start start
     :stop stop}))

(defn use-atom [sub-atom]
  (let [sub (hooks/use-memo [sub-atom]
              {:get-current-value #(deref sub-atom)
               :subscribe (fn [f]
                            (let [k (gensym "subscribe")]
                              (add-watch sub-atom k f)
                              #(remove-watch sub-atom k)))})]
    (hooks/use-subscription sub)))

(declare tracker-start)
(deftype TaskInstance [task !state]
  IFn
  (-invoke [_ failure success]
    (assert (= (:state @!state) :waiting) "Tracked task should be started only once.")
    (tracker-start !state task failure success))

  IDeref
  (-deref [_] @!state)

  IWatchable
  (-notify-watches [_ oldval newval]
    (-notify-watches !state oldval newval))
  (-add-watch [_ key f]
    (-add-watch !state key f))
  (-remove-watch [_ key]
    (-remove-watch !state key)))

(defn- instance-state-map [state]
  {:state state
   :started? (not= state :waiting) ; rename to idle?
   :canceled? (= state :canceled)
   :error? (= state :error)
   :finished? (or (= state :canceled)
                  (= state :finished)
                  (= state :error))
   :successful? (= state :finished)}) ; proably rename :finished to success

(defn tracker-start [!state task success failure]
  (swap! !state merge
         (instance-state-map :running)
         {:start-time (js/Date.now)})
  (let [cancel (task (fn [x]
                       (swap! !state merge
                              (instance-state-map :finished)
                              {:value x
                               :end-time (js/Date.now)})
                       (success x))
                     (fn [e]
                       (if (-> e ex-data :cancelled)
                         (swap! !state merge
                                (instance-state-map :canceled)
                                {:end-time (js/Date.now)})
                         (swap! !state merge
                                (instance-state-map :error)
                                {:error e
                                 :end-time (js/Date.now)}))
                       (failure e)))]
    cancel))

(defn make-tracker [task]
  (let [!state (atom (assoc (instance-state-map :waiting)
                            :perform-time (js/Date.now)
                            :start-time nil
                            :end-time nil
                            :value nil
                            :error nil))]
    (->TaskInstance task !state)))

(defn format-task-status [status]
  (if (= status :running)
    "running"
    "idle"))

(defnc Tracker [{:keys [id tracker time scale-x waiting?]}]
  (let [{:keys [state perform-time start-time end-time]} (use-atom tracker)
        state (if (and (= :waiting state) (not (waiting? tracker)))
                :dropped
                state)
        color (get colors (mod id (count colors)))
        y (* (mod id 6) track-height)]
    (d/g {:height track-height}
         (when start-time
           (let [x (scale-x start-time)
                 width (max 0 (- (scale-x (or end-time time))
                                 x))]
             (d/rect {:x (str x "%")
                      :y y
                      :height track-height
                      :width (str width "%")
                      :stroke "black"
                      :fill color
                      :fill-opacity "0.3"})))
         (let [x (scale-x perform-time)]
           (<>
            (d/text {:x (str (+ x 0.5) "%")
                     :y (+ y 15)
                     :font-family "sans-serif"
                     :fill color
                     :font-size 14
                     :text-decoration (if (#{:canceled :dropped} state) "line-through" "none")
                     :font-style (if start-time "normal" "italic")}
                    (gstr/capitalize (name state)))
            (d/line {:x1 (str x "%") :x2 (str x "%") :y1 y :y2 (+ y 20) :stroke color}))))))

(defnc Graph [{:keys [perform]}]
  (let [{:keys [start-time time start stop]} (use-animation)
        [!trackers] (hooks/use-state #(atom []))
        perform-with-tracker (fn [task]
                               (let [tracker-task (make-tracker task)]
                                 (perform tracker-task)
                                 tracker-task))
        trackers (use-atom !trackers)
        time-elapsed (max (- time start-time) 10000)
        scale-x (fn [x]
                  (* (/ (- x start-time)
                        time-elapsed)
                     100))
        perform! (fn []
                   (start)
                   (swap! !trackers conj
                          (perform-with-tracker (m/sleep 1500))))
        clear-timeline! (fn []
                          (stop)
                          (reset! !trackers []))
        waiting? (set (core/waiting perform))]
    (d/div
     (d/div
      (d/button {:on-click perform!}
                "Perform")
      (d/button {:on-click clear-timeline!}
                "Clear Timeline"))
        ; (d/button "Cancel all"))
     (d/svg {:style {:width "100%"}}
            (for [[id tracker] (map-indexed list trackers)]
              ($ Tracker {:key id
                          :id id
                          :tracker tracker
                          :time time
                          :scale-x scale-x
                          :waiting? waiting?}))
            (let [x (scale-x time)]
              (d/line {:x1 (str x "%")
                       :x2 (str x "%")
                       :y1 0
                       :y2 "100%"
                       :stroke "black"}))))))

(defnc DefiningTasks []
  (let [[status set-status] (hooks/use-state nil)
        [perform] (hooks/use-state #(core/unbounded))
        task (m/sp
              (set-status "Gimme one second...")
              (m/? (m/sleep 1000))
              (set-status "Gimme one more second...")
              (m/? (m/sleep 1000))
              (set-status "OK, I'm done."))]
    (d/div
     (d/button {:on-click #(perform task)}
               "Wait A Few Seconds")
     (d/span status))))

(defnc CancelationDemo []
  (let [[counter set-counter] (hooks/use-state 0)
        [cancel-most-recent set-cancel-most-recent!] (hooks/use-state nil)
        [perform] (hooks/use-state #(core/unbounded))
        [!tracker set-tracker!] (hooks/use-state #(atom {}))
        perform! (fn [task]
                   (perform (fn [s f]
                              (let [tracker (make-tracker task)
                                    cancel (tracker s f)]
                                (set-tracker! tracker)
                                (set-cancel-most-recent! cancel)
                                cancel))))
        task (m/sp
              (try
                (set-counter inc)
                (m/? m/never)
                (finally
                  (set-counter dec))))
        {:keys [state]} (use-atom !tracker)]
    (d/div
     (d/div "Running tasks: " counter)
     (d/button {:on-click #(perform! task)}
               "Perform Task")
     (when (pos? counter)
       (d/button {:on-click #(core/cancel perform)} "Cancel All"))
     (when (= state :running)
       (d/button {:on-click #(cancel-most-recent)}
                 "Cancel Most Recent")))))

(defnc ErrorsVsCancelation []
  (let [[num-completions set-num-completions] (hooks/use-state 0)
        [num-errors set-num-errors] (hooks/use-state 0)
        [num-finallys set-num-finallys] (hooks/use-state 0)
        [perform] (hooks/use-state #(core/restartable))
        [!tracker set-tracker!] (hooks/use-state #(atom {}))
        perform (fn [task]
                  (let [tracker (make-tracker task)]
                    (set-tracker! tracker)
                    (perform tracker)))
        task (fn [error?]
               (m/sp
                (try
                  (m/? (m/sleep 1000))
                  ;; Difference to ember-concurrency, because it uses generators the completion counter can be outside of the try-catch
                  (set-num-completions inc)
                  (when error?
                    (throw (js/Error. "Boom")))
                  (catch :default e
                    ;; This is difference to ember-concurrency, which does not consider cancellation an error
                    ;; Maybe reconsider later
                    (when-not (-> e ex-data :cancelled)
                      (set-num-errors inc)))
                  (finally
                    (set-num-finallys inc)))))
        {:keys [state]} (use-atom !tracker)]
    (d/div
     (d/button {:on-click #(perform (task false))}
               "Run to Completion")
     (d/button {:on-click #(perform (task true))}
               "Throw an Error")
     (d/ul
      (d/li "Task state: " (format-task-status state))
      (d/li "Completions: " num-completions)
      (d/li "Errors: " num-errors)
      (d/li "Finally block runs: " num-finallys)))))

(defn make-demo-tasks [set-status!]
  (let [grandchild-task (make-tracker
                         (m/sp
                          (set-status! "3. Grandchild: one moment...")
                          (m/? (m/sleep 1000))
                          "Hello"))
        child-task (make-tracker
                    (m/sp
                     (set-status! "2. Child: one moment...")
                     (m/? (m/sleep 1000))
                     (let [value (m/? grandchild-task)]
                       (set-status! (str "4. Child: grandchild says \"" value "\"")))
                     (m/? (m/sleep 1000))
                     "What's up"))
        parent-task (make-tracker
                     (m/sp
                      (set-status! "1. Parent: one moment...")
                      (m/? (m/sleep 1000))
                      (let [value (m/? child-task)]
                        (set-status! (str "5. Parent: child says \"" value "\"")))
                      (m/? (m/sleep 1000))
                      (set-status! "6. Done!")))]
    {:grandchild-task grandchild-task
     :child-task child-task
     :parent-task parent-task}))

(defnc ChildTasks []
  (let [[status set-status!] (hooks/use-state "Waiting to start")
        [tasks set-tasks!] (hooks/use-state (fn [] (make-demo-tasks set-status!)))
        [perform] (hooks/use-state #(core/restartable))
        perform! (fn []
                   (let [{:keys [parent-task] :as tasks} (make-demo-tasks set-status!)]
                     (set-tasks! tasks)
                     (perform parent-task)))
        {:keys [parent-task child-task grandchild-task]} tasks
        {parent-task-state :state} (use-atom parent-task)
        {child-task-state :state} (use-atom child-task)
        {grandchild-task-state :state} (use-atom grandchild-task)]
    (d/div
     (d/div status)
     (d/ul
      (d/li "Parent Task: " (format-task-status parent-task-state))
      (d/li "Child Task: " (format-task-status child-task-state))
      (d/li "Grandchild Task: " (format-task-status grandchild-task-state)))
     (d/button {:on-click #(perform!)}
               (if (= parent-task-state :running)
                 "Restart Parent Task"
                 "Perform Parent Task")))))

(defnc App []
  (d/div
   (d/h1 "Welcome!")
   (d/h2 "Defining Tasks")
   ($ DefiningTasks)
   (d/h2 "Cancelation")
   ($ CancelationDemo)
   (d/h2 "Handling Errors")
   ($ ErrorsVsCancelation)
   (d/h2 "Child Tasks")
   ($ ChildTasks)
   (d/h2 "Task Modifiers")
   (d/h3 "unbounded: Tasks run concurrently")
   (d/div
    ($ Graph {:perform (core/unbounded)}))
   (d/h3 "restartable")
   (d/div
    ($ Graph {:perform (core/restartable)}))
   (d/h3 "enqueue")
   (d/div
    ($ Graph {:perform (core/enqueued)}))
   (d/h3 "drop")
   (d/div
    ($ Graph {:perform (core/dropping)}))
   (d/h3 "keepLatest")
   (d/div
    ($ Graph {:perform (core/keeping-latest)}))
   (d/h2 "Using max-concurrency")
   (d/h3 "restartable with max-concurrency: 3")
   (d/div
    ($ Graph {:perform (core/restartable {:max-concurrency 3})}))
   (d/h3 "enqueue with max-concurrency: 3")
   (d/div
    ($ Graph {:perform (core/enqueued {:max-concurrency 3})}))
   (d/h3 "drop with max-concurrency: 3")
   (d/div
    ($ Graph {:perform (core/dropping {:max-concurrency 3})}))
   (d/h3 "keepLatest with max-concurrency: 3")
   (d/div
    ($ Graph {:perform (core/keeping-latest {:max-concurrency 3})}))))

(defn ^:export main
  []
  (rdom/render ($ App) (js/document.getElementById "app")))

