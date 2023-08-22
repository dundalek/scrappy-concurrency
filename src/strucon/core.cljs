(ns strucon.core)

(defprotocol Waiting
  (waiting [_]))

(defprotocol Cancellable
  (cancel [_]))

(declare unbounded-start)

(deftype Unbounded [^:mutable instances
                    ^:mutable success]
  IFn
  (-invoke [this task]
    (unbounded-start this task))
  (-invoke [this s f]
    (set! (.-success this) s))

  Waiting
  (waiting [_] [])

  Cancellable
  (cancel [_]
    (doseq [cancel instances]
      (cancel))
    (set! instances #{})))

(defn unbounded-start [^Unbounded this task]
  (let [!instance (atom nil)
        cancel (task (fn on-success []
                       (set! (.-instances this)
                             (disj (.-instances this) @!instance))
                       (when (and (empty? (.-instances this))
                                  (ifn? (.-success this)))
                         ((.-success this))))
                     (fn on-fail [e]))]
                       ;; todo, probably cancel others and propagate
    (reset! !instance cancel)
    (set! (.-instances this)
          (conj (.-instances this) cancel))
    ;; would return be useful for anything?
    nil))

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

       Waiting
       (waiting [_] [])

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
         (swap! !queue conj task)
         (enqueued-maybe-start max-concurrency !current !queue)
         nil)

       Waiting
       (waiting [_] (seq @!queue))

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
         (when (< (count @!current) max-concurrency)
           (start-task! !current task
                        (fn []
                          (when (empty? @!current)
                            (when (ifn? @!s)
                              (@!s))))
                        (fn []))))
       (-invoke [_ s f]
         (reset! !s s))

       Waiting
       (waiting [_] [])

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
         (reset! !waiting task)
         (keeping-latest-maybe-start max-concurrency !current !waiting !s))
       (-invoke [_ s f]
         (reset! !s s))

       Waiting
       (waiting [_]
         (if @!waiting
           [@!waiting]
           []))

       Cancellable
       (cancel [_]
         (reset! !waiting nil)
         (cancel-current! !current))))))

