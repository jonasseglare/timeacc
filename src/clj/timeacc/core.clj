(ns timeacc.core
  (:import [timeacc Root IAcc]))

(defn root []
  (Root.))

(defn unsafe-acc [^Root r k]
  (.getUnsafeAcc r k))

(defn accumulate-nano-seconds-since [^IAcc acc start]
  (.accumulateNanoSecondsSince acc start))

(defn accumulate-nano-seconds [^IAcc acc dur]
  (.accumulateNanoSeconds acc dur))

(defn reset [^Root r]
  (.reset r))

(defn counter [^IAcc acc]
  (.getCounter acc))

(defn total-time-seconds [^IAcc acc]
  (* 1.0e-9 (.getTotalTimeNs acc)))

(defn avg-time-seconds [^IAcc acc]
  (/ (total-time-seconds acc)
     (counter acc)))

(defn acc-map [^Root r]
  (into {}
        (remove (fn [[_ v]] (zero? (counter v))))
        (.getAccMap r)))

(defn report [^Root r]
  (println "Key                Counter    Total (s)   Average (s)")
  (println "=============== ========== ============ =============")
  (doseq [[k v] (sort-by key (acc-map r))]
    (println (format "%s %24d %12.6f %12.6f"
                     (name k)
                     (counter v)
                     (total-time-seconds v)
                     (avg-time-seconds v)))))

(comment

  (def the-root (root))
  (def acc (unsafe-acc the-root :a))
  (def start (System/nanoTime))
  (accumulate-nano-seconds-since acc start)

  (reset the-root)


  )
