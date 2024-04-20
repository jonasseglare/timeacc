(ns timeacc.core
  (:require [clojure.string :as str])
  (:import [timeacc Root IAcc StopWatch]))

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

(defn render-table [rows]
  (let [col-widths-f (fn [& elements]
                     (apply max (map count elements)))
        col-widths (apply map col-widths-f rows)]
    (str/join "\n"
              (for [row rows]
                (str/join
                 "   "
                 (for [[i col-width element] (map vector (range) col-widths row)]
                   (let [padw (- col-width (count element))
                         padding (apply str (repeat padw " "))]
                     (if (zero? i)
                       (str element padding)
                       (str padding element)))))))))

(defmacro measure [acc & body]
  `(let [start-ns# (System/nanoTime)
         result# (do ~@body)]
     (accumulate-nano-seconds-since ~acc start-ns#)
     result#))

(defn stop-watch [^IAcc acc]
  (StopWatch. acc))

(defn start [^StopWatch w]
  (.start w))

(defn stop [^StopWatch w]
  (.stop w))

(defmacro with-pause [watch & expr]
  `(let [_# (stop ~watch)
         result# (do ~@expr)]
     (start ~watch)
     result#))

(defmacro with-watch [watch & expr]
  #_`(let [_ (start ~watch)
         result# (do ~@expr)]
     (stop ~watch)
     result#))

(defn measure-xform [acc xform]
  (let [w (stop-watch acc)]
    (fn [step]
      (let [timed-step (fn
                         ([] (with-pause w (step)))
                         ([a] (with-pause w (step a)))
                         ([a b] (with-pause w (step a b)))
                         ([a b c] (with-pause w (step a b c)))
                         ([a b c d] (with-pause w (step a b c d)))
                         ([a b c d e] (with-pause w (step a b c d e)))
                         ([a b c d e f] (with-pause w (step a b c d e f)))
                         ([a b c d e f g] (with-pause w (step a b c d e f g)))
                         ([a b c d e f g h] (with-pause w (step a b c d e f g h))))
            new-step (xform timed-step)]
        (fn
          ([] (with-watch w (new-step)))
          ([a] (with-watch w (new-step a)))
          ([a b] (with-watch w (new-step a b)))
          ([a b c] (with-watch w (new-step a b c)))
          ([a b c d] (with-watch w (new-step a b c d)))
          ([a b c d e] (with-watch w (new-step a b c d e)))
          ([a b c d e f] (with-watch w (new-step a b c d e f)))
          ([a b c d e f g] (with-watch w (new-step a b c d e f g)))
          ([a b c d e f g h] (with-watch w (new-step a b c d e f g h))))))))

(defn report [^Root r]
  (->> r
       acc-map
       (sort-by key)
       (into [["KEY" "COUNTER" "TOTAL (s)" "AVERAGE (s)"]]
             (map (fn [[k v]]
                    [(name k)
                     (format "%d" (counter v))
                     (format "%.6f" (total-time-seconds v))
                     (format "%.6f" (avg-time-seconds v))])))
       render-table
       println))

(defn demo []
  (let [root (root)
        a (unsafe-acc root :search-batch-fn)
        b (unsafe-acc root :backend-xform)]
    (measure b
      (dotimes [_ 100]
        (let [x (System/nanoTime)]
          (Thread/sleep 10)
          (accumulate-nano-seconds-since a x))))
    (report root)))

(defn xform-demo []
  (let [root (root)
        a (unsafe-acc root :search-batch-fn)
        b (unsafe-acc root :backend-xform)
        result (into []
                     (comp (measure-xform a (map (fn [x]
                                                   (Thread/sleep 100)
                                                   (* x 2))))
                           (measure-xform b (map (fn [x]
                                                   (Thread/sleep 100)
                                                   (inc x)))))
                     [1 2 3 4 5])]
    (println "Result" result)
    (report root)))

(comment

  (def the-root (root))
  (def acc (unsafe-acc the-root :a))
  (def start (System/nanoTime))
  (accumulate-nano-seconds-since acc start)

  (def the-watch (stop-watch acc))
  (start the-watch)
  (stop the-watch)

  (report the-root)

  (reset the-root)


  )
