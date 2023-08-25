(ns strucon.core
  (:import
   (missionary Cancelled)))

(defn cancelled [msg]
  (Cancelled. msg))

(defn cancelled? [e]
  (instance? Cancelled e))

(defn cancel-shield [task]
  (fn [s f]
    (task s (fn [e]
              (if (instance? Cancelled e)
                (s nil)
                (f e))))))

(defprotocol Cancellable
  (cancel [_]))

(defprotocol Droppable
  (drop! [_]))

(defn- observable-task [task drop-task]
  (let [!drop-or-cancel (atom nil)
        !watcher-s (atom nil)
        !watcher-f (atom nil)
        !dropped (atom false)
        wrapped-task (reify
                       IFn
                       (-invoke [_ s f]
                         (let [cancel (task
                                       (fn [x]
                                          ;; set bound state here?
                                         (when-some [watcher @!watcher-s]
                                           (watcher x))
                                         (s x))
                                       (fn [e]
                                         ;; set something like dropped also here?
                                         (when-some [watcher @!watcher-f]
                                           (watcher e))
                                         (f e)))]
                           (reset! !drop-or-cancel cancel)
                           cancel))

                       Droppable
                       (drop! [this]
                         (reset! !dropped true)
                         (drop-task this)
                         (when (satisfies? Droppable task)
                           (drop! task))
                         (when-some [watcher @!watcher-f]
                           ;; pass some value for cancelled?
                           (watcher))))
        observer-task (fn [s f]
                        (if @!dropped
                          (do
                            (f (cancelled "Dropped"))
                            (fn nop []))
                          (do
                            (reset! !watcher-s s)
                            (reset! !watcher-f f)
                            (fn [] (@!drop-or-cancel)))))]
    (reset! !drop-or-cancel
            (fn [] (drop! wrapped-task)))
    [wrapped-task observer-task]))

(declare unbounded-start)

(deftype Unbounded [^:mutable instances
                    ^:mutable success]
  IFn
  (-invoke [this task]
    (unbounded-start this task))
  (-invoke [this s f]
    (set! (.-success this) s))

  Cancellable
  (cancel [_]
    (doseq [cancel instances]
      (cancel))
    (set! instances #{})))

(defn unbounded-start [^Unbounded this task]
  (let [!instance (atom nil)
        [wrapped-task observer-task] (observable-task task nil)
        cancel (wrapped-task
                (fn on-success []
                  (set! (.-instances this)
                        (disj (.-instances this) @!instance))
                  (when (and (empty? (.-instances this))
                             (ifn? (.-success this)))
                    ((.-success this))))
                (fn on-fail [e]
                  (set! (.-instances this)
                        (disj (.-instances this) @!instance))))]
                       ;; todo, probably cancel others and propagate
    (reset! !instance cancel)
    (set! (.-instances this)
          (conj (.-instances this) cancel))
    observer-task))

(defn unbounded []
  (->Unbounded #{} nil))

(defn- remove-task! [!current x]
  (swap! !current
         (fn [coll]
           (filterv (complement #{x}) coll))))

(defn- start-task! [!current task s f]
  (let [!instance (atom nil)
        instance (task (fn [x]
                         (remove-task! !current @!instance)
                         (s x))
                       (fn [e]
                         (remove-task! !current @!instance)
                         (f e)))]
    (reset! !instance instance)
    (swap! !current conj instance)
    nil))

(defn- cancel-current! [!current]
  (doseq [cancel @!current]
    (cancel))
  (reset! !current []))

(defn restartable
  ([] (restartable {}))
  ([{:keys [max-concurrency]}]
   (let [max-concurrency (or max-concurrency 1)
         !current (atom [])
         !s (atom nil)]
     (assert (pos? max-concurrency))
     (reify
       IFn
       (-invoke [_ task]
         (when (<= max-concurrency (count @!current))
           (let [cancel (first @!current)]
             (remove-task! !current cancel)
             (cancel)))
         (start-task! !current task
                      (fn []
                        (when (empty? @!current)
                          (when (ifn? @!s)
                            (@!s))))
                      (fn [])))
       (-invoke [_ s f]
         (reset! !s s))

       Cancellable
       (cancel [_] (cancel-current! !current))))))

(defn enqueued-maybe-start [max-concurrency !current !queue]
  (when (< (count @!current) max-concurrency)
    (when-some [task (peek @!queue)]
      (swap! !queue pop)
      (start-task! !current task
                   (fn [] (enqueued-maybe-start max-concurrency !current !queue))
                   (fn [])))))

(defn enqueued
  ([] (enqueued {}))
  ([{:keys [max-concurrency]}]
   (let [max-concurrency (or max-concurrency 1)
         !current (atom [])
         !queue (atom #queue [])]
     (assert (pos? max-concurrency))
     (reify
       IFn
       (-invoke [_ task]
         (let [[wrapped-task observer-task] (observable-task
                                             task
                                             (fn [dropped-task]
                                               (swap! !queue
                                                      (fn [q]
                                                        (into #queue []
                                                              (filter (complement #{dropped-task}))
                                                              q)))))]
           (swap! !queue conj wrapped-task)
           (enqueued-maybe-start max-concurrency !current !queue)
           observer-task))

       Cancellable
       (cancel [_]
         (reset! !queue #queue [])
         (cancel-current! !current))))))

(defn dropping
  ([] (dropping {}))
  ([{:keys [max-concurrency]}]
   (let [max-concurrency (or max-concurrency 1)
         !current (atom [])
         !s (atom nil)]
     (assert (pos? max-concurrency))
     (reify
       IFn
       (-invoke [_ task]
         (cond
           (< (count @!current) max-concurrency)
           (start-task! !current task
                        (fn []
                          (when (empty? @!current)
                            (when (ifn? @!s)
                              (@!s))))
                        (fn []))

           (satisfies? Droppable task)
           (drop! task)))
       (-invoke [_ s f]
         (reset! !s s))

       Cancellable
       (cancel [_] (cancel-current! !current))))))

(defn- keeping-latest-maybe-start [max-concurrency !current !waiting !s]
  (when (< (count @!current) max-concurrency)
    (if-some [task @!waiting]
      (do
        (reset! !waiting nil)
        (start-task! !current task
                     (fn [] (keeping-latest-maybe-start max-concurrency !current !waiting !s))
                     (fn [])))
      (when (empty? @!current)
        (when (ifn? @!s)
          (@!s))))))

(defn keeping-latest
  ([] (keeping-latest {}))
  ([{:keys [max-concurrency]}]
   (let [max-concurrency (or max-concurrency 1)
         !current (atom [])
         !waiting (atom nil)
         !s (atom nil)]
     (assert (pos? max-concurrency))
     (reify
       IFn
       (-invoke [_ task]
         (when-some [waiting-task @!waiting]
           (drop! waiting-task))
         (let [[wrapped-task observer-task] (observable-task
                                             task
                                             (fn [dropped-task]
                                               (when (identical? dropped-task @!waiting)
                                                 (reset! !waiting nil))))]
           (reset! !waiting wrapped-task)
           (keeping-latest-maybe-start max-concurrency !current !waiting !s)
           observer-task))
       (-invoke [_ s f]
         (reset! !s s))

       Cancellable
       (cancel [_]
         (reset! !waiting nil)
         (cancel-current! !current))))))
