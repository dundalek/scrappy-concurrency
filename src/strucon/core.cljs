(ns strucon.core)

(declare unbounded-start)

(deftype Unbounded [^:mutable instances
                    ^:mutable success]
  IFn
  (-invoke [this task]
    (unbounded-start this task))
  (-invoke [this s f]
    (set! (.-success this) s)))

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
                       f)]
    (reset! !instance instance)
    (swap! !current conj instance)
    nil))

(defn restartable
  ([] (restartable {}))
  ([{:keys [max-concurrency]}]
   (let [max-concurrency (or max-concurrency 1)
         !current (atom [])
         !s (atom nil)]
     (assert (pos? max-concurrency))
     (fn
       ([task]
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
       ([s f]
        (reset! !s s))))))

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
     (fn [task]
       (swap! !queue conj task)
       (enqueued-maybe-start max-concurrency !current !queue)
       nil))))

(defn dropping
  ([] (dropping {}))
  ([{:keys [max-concurrency]}]
   (let [max-concurrency (or max-concurrency 1)
         !current (atom [])
         !s (atom nil)]
     (assert (pos? max-concurrency))
     (fn
       ([task]
        (when (< (count @!current) max-concurrency)
          (start-task! !current task
                       (fn []
                         (when (empty? @!current)
                           (when (ifn? @!s)
                             (@!s))))
                       (fn []))))
       ([s f]
        (reset! !s s))))))

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
     (fn
       ([task]
        (reset! !waiting task)
        (keeping-latest-maybe-start max-concurrency !current !waiting !s)
        nil)
       ([s f]
        (reset! !s s))))))
