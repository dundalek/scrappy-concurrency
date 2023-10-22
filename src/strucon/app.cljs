(ns strucon.app
  (:require
   ["react-dom" :as rdom]
   [clojure.string :as str]
   [goog.string :as gstr]
   [helix.core :refer [$ <>]]
   [helix.dom :as d]
   [helix.hooks :as hooks]
   [missionary.core :as m]
   [strucon.core :as core]
   [strucon.protocols :as protocols]
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

(deftype TaskInstance [task !state]
  IFn
  (-invoke [_ failure success]
    (task failure success))

  IDeref
  (-deref [_] @!state)

  IWatchable
  (-notify-watches [_ oldval newval]
    (-notify-watches !state oldval newval))
  (-add-watch [_ key f]
    (-add-watch !state key f))
  (-remove-watch [_ key]
    (-remove-watch !state key))

  protocols/Droppable
  (drop! [_] (swap! !state merge (instance-state-map :dropped))))

(defn state-tracked-task [task !state]
  (swap! !state merge
         (instance-state-map :waiting)
         {:value nil
          :error nil})
  (fn [success failure]
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

(defn time-tracked-task [task !state]
  (swap! !state assoc
         :perform-time (js/Date.now)
         :start-time nil
         :end-time nil)
  (fn [success failure]
    (swap! !state assoc :start-time (js/Date.now))
    (task (fn [x]
            (swap! !state assoc :end-time (js/Date.now))
            (success x))
          (fn [e]
            (swap! !state assoc :end-time (js/Date.now))
            (failure e)))))

(defn tracked-task [task !state]
  (-> task
      (state-tracked-task !state)
      (time-tracked-task !state)))

(defn make-tracker [task]
  (let [!state (atom {})
        wrapped-task (tracked-task task !state)
        wrapped-task (fn [s f]
                       (assert (= (:state @!state) :waiting)
                               "Tracked task should be started only once. Create new tracker instance for each perform")
                       (wrapped-task s f))]
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

(defn format-task-status [status]
  (if (= status :running)
    "running"
    "idle"))

(defnc Tracker [{:keys [id tracker time scale-x num-tracks]}]
  (let [{:keys [state perform-time start-time end-time]} (use-atom tracker)
        color (get colors (mod id (count colors)))
        y (* (mod id num-tracks) track-height)]
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

(defnc TaskGraph [{:keys [animation trackers num-tracks] :or {num-tracks 6}}]
  (let [{:keys [start-time time]} animation
        time-elapsed (max (- time start-time) 10000)
        scale-x (fn [x]
                  (* (/ (- x start-time)
                        time-elapsed)
                     100))]
    (d/svg {:style {:width "100%"}}
           (for [[id tracker] (map-indexed list trackers)]
             ($ Tracker {:key id
                         :id id
                         :tracker tracker
                         :time time
                         :scale-x scale-x
                         :num-tracks num-tracks}))
           (let [x (scale-x time)]
             (d/line {:x1 (str x "%")
                      :x2 (str x "%")
                      :y1 0
                      :y2 "100%"
                      :stroke "black"})))))

(defnc Graph [{:keys [perform]}]
  (let [{:keys [start stop] :as animation} (use-animation)
        [!trackers] (hooks/use-state #(atom []))
        [running-count set-running-count!] (hooks/use-state 0)
        perform-with-tracker (fn [task]
                               (let [tracker-task (-> task
                                                      (make-running-tracker set-running-count!)
                                                      (make-tracker))]
                                 (perform tracker-task)
                                 tracker-task))
        trackers (use-atom !trackers)
        perform! (fn []
                   (start)
                   (swap! !trackers conj
                          (perform-with-tracker (m/sleep 1500))))
        clear-timeline! (fn []
                          (stop)
                          (core/cancel perform)
                          (reset! !trackers []))]
    (d/div
     (d/div
      (d/button {:on-click perform!}
                "Perform")
      (d/button {:on-click clear-timeline!}
                "Clear Timeline")
      (when (pos? running-count)
        (d/button {:on-click #(core/cancel perform)}
                  "Cancel all")))
     ($ TaskGraph {:animation animation
                   :trackers trackers}))))

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
                    (when-not (core/cancelled? e)
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

(def words ["ember" "tomster" "swag" "yolo" "turbo" "ajax"])
(def loading-colors ["#ff8888" "#88ff88" "#8888ff"])

(def percents
  (m/ap
   (loop [percent 0]
     (m/amb
      percent
      (if (< percent 100)
        (do
          (m/? (m/sleep (+ 100 (rand-int 100))))
          (let [new-percent (min 100 (+ percent (rand-int 20)))]
            (recur new-percent)))
        (m/amb))))))

(defn load-word-task [on-change]
  (m/sp
   (m/? (m/reduce (fn [_ p] (on-change p))
                  nil percents))
   (rand-nth words)))

(defnc AwaitingMultipleChildTasks []
  (let [[percents set-percents!] (hooks/use-state nil)
        [status set-status!] (hooks/use-state "Waiting...")
        [perform-parent] (hooks/use-state #(core/restartable))
        [perform-child] (hooks/use-state #(core/enqueued {:max-concurrency 3}))
        run (fn [method]
              (perform-parent
               (m/sp
                (set-status! "Waiting for child tasks to complete...")
                (set-percents! (vec (repeat 5 {:percent 0 :word nil})))
                (let [tasks (->> (range 5)
                                 (map (fn [i]
                                        (core/cancel-shield
                                         (perform-child
                                          (m/sp
                                           (let [result (m/? (load-word-task
                                                              #(set-percents! assoc-in [i :percent] %)))]
                                             (set-percents! assoc-in [i :word] result)
                                             result)))))))
                      words (case method
                              :join (m/? (apply m/join vector tasks))
                              :race [(m/? (apply m/race tasks))])]
                  (set-status! (str "Done: " (str/join ", " words)))))))]
    (d/div
     (d/div status)
     (d/button {:on-click #(run :join)}
               "join")
     (d/button {:on-click #(run :race)}
               "race")
     (d/div
      (for [[{:keys [percent word]} i color] (map list percents (range) (cycle loading-colors))]
        (d/div {:key i
                :style {:background-color color
                        :width (str percent "%")
                        :transition (when (pos? percent) "width 0.5s")
                        :border-radius "3px"
                        :line-height "40px"
                        :white-space "nowrap"
                        :margin "10px 0"}}
               (str "Progress: " percent "%")
               (when word
                 (str " Word: " word))))))))

;; Happy Eyballs implementation from:
;; https://github.com/leonoel/missionary/blob/master/doc/guides/happy_eyeballs.md
(defn attempt [chosen delay close! connectors]
  (if-some [[connector & connectors] connectors]
    ;; assigning this dataflow variable will trigger next attempt
    (let [trigger (m/dfv)]
      (m/race
        ;; try to connect to endpoing
       (m/sp (try (let [x (m/? connector)
                        y (chosen x)]
                     ;; if another attempt succeeded, close socket before terminate
                    (when-not (identical? x y) (close! x)) y)
                  (catch :default e
                     ;; if attempt fails early, trigger next attempt immediately
                    (trigger nil)
                    (throw e))))
        ;; trigger next attempt if still pending after delay
       (m/sp (m/? (m/sleep delay))
             (trigger nil)
             (m/? m/never))
        ;; wait for trigger and recursively try next endpoints
       (m/sp (m/? trigger)
             (m/? (attempt chosen delay close! connectors)))))
    ;; no more endpoint, return a failing task
    (m/race)))

(defn happyeyeballs [delay close! connectors]
  (m/sp
   (try
     (m/? (attempt (m/dfv) delay close! connectors))
     (catch :default _
       (throw (ex-info "Unable to reach target."
                       {:delay delay
                        :close! close!
                        :connectors connectors}))))))

(defnc HappyEyeballsGraph []
  (let [{:keys [start stop] :as animation} (use-animation)
        [!trackers] (hooks/use-state #(atom []))
        [status set-status!] (hooks/use-state "Idle")
        [perform] (hooks/use-state #(core/restartable))
        trackers (use-atom !trackers)
        perform! (fn []
                   (start)
                   (let [connectors (->> (range 1 5)
                                         (mapv (fn [i]
                                                 (make-tracker
                                                  (m/sp
                                                   (m/? (m/sleep (+ 300 (rand-int 1500))))
                                                   (if (< (rand) 0.5)
                                                     (throw (js/Error. (str "Failed " i)))
                                                     (str "Chosen " i)))))))]
                     (swap! !trackers into connectors)
                     (perform (m/sp
                               (set-status! "Running")
                               (set-status! (try
                                              (m/? (happyeyeballs 500 (fn [_]) connectors))
                                              (catch :default _
                                                "All failed")))))))
        clear-timeline! (fn []
                          (stop)
                          (core/cancel perform)
                          (reset! !trackers [])
                          (set-status! "Idle"))]
    (d/div
     (d/div
      (d/div status)
      (d/button {:on-click perform!}
                "Perform")
      (d/button {:on-click clear-timeline!}
                "Clear Timeline")
      #_(when (pos? running-count)
          (d/button {:on-click #(core/cancel perform)}
                    "Cancel all")))
     ($ TaskGraph {:animation animation
                   :trackers trackers
                   :num-tracks 4}))))

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
   (d/h2 "Awaiting Multiple Child Tasks")
   ($ AwaitingMultipleChildTasks)
   (d/h2 "Happy Eyeballs")
   ($ HappyEyeballsGraph)
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
    ($ Graph {:perform (core/keeping-latest {:max-concurrency 3})}))
   (d/h3 "Task Groups alternative: apply constraints to multiple tasks")
   ;; https://youtu.be/VEzVDOmY-dc?si=uUMHF1ofO_EtZAFw&t=849
   (let [perform (core/restartable)]
     (d/div
      ($ Graph {:perform perform})
      ($ Graph {:perform perform})))))

(defn ^:export main
  []
  (rdom/render ($ App) (js/document.getElementById "app")))

