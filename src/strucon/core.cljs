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

(defn restartable []
  (let [!current (atom nil)
        !s (atom nil)]
    (fn
      ([task]
       (when-some [cancel @!current]
         (cancel))
       (reset! !current
               (task (fn []
                       (reset! !current nil)
                       (when (ifn? @!s)
                         (@!s)))
                     (fn []))))
      ([s f]
       (reset! !s s)))))

(defn enqueued-maybe-start [!current !queue]
  (when-not @!current
    (when-some [task (peek @!queue)]
      (swap! !queue pop)
      (reset! !current
              (task (fn []
                      (reset! !current nil)
                      (enqueued-maybe-start !current !queue))
                    (fn []))))))

(defn enqueued []
  (let [!current (atom nil)
        !queue (atom #queue [])]
    (fn [task]
      (swap! !queue conj task)
      (enqueued-maybe-start !current !queue))))

(defn dropping []
  (let [!current (atom nil)
        !s (atom nil)]
    (fn
      ([task]
       (when-not @!current
         (reset! !current
                 (task (fn []
                         (reset! !current nil)
                         (when (ifn? @!s)
                           (@!s)))
                       (fn [])))))
      ([s f]
       (reset! !s s)))))

(defn- keeping-latest-maybe-start [!current !waiting !s]
  (when-not @!current
    (if-some [task @!waiting]
      (do
        (reset! !waiting nil)
        (reset! !current
                (task (fn []
                        (reset! !current nil)
                        (keeping-latest-maybe-start !current !waiting !s))
                      (fn []))))
      (when (ifn? @!s)
        (@!s)))))

(defn keeping-latest []
  (let [!current (atom nil)
        !waiting (atom nil)
        !s (atom nil)]
    (fn
      ([task]
       (reset! !waiting task)
       (keeping-latest-maybe-start !current !waiting !s))
      ([s f]
       (reset! !s s)))))
