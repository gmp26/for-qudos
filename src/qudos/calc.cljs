(ns ^:figwheel-always qudos.calc
  (:require [rum.core :as rum]
            [clojure.string :as s]
            [cljs.pprint :as pp]
            [cljx-sampling.random :as random]
            [cljx-sampling.core :refer [sample]]))

(enable-console-print!)
;; See https://github.com/ashenfad/cljx-sampling

;(def rng (random/create 45))

(defn rand01 [rng] (random/next-double! rng 1))

(defn rand-n [rng n] (* n (random/next-double! rng)))

(def ub-risks [0.0005456 0.0009988 0.0015798 0.0023618 0.0033583
               0.0044388 0.0056236 0.0068915 0.0084389 0.0102027
               0.0123412 0.0151487 0.0184223 0.0226536 0.0278246
               0.0348422 0.0463867 0.0640427 0.1017762 0.6340773])

(def centiles (partition 2 1 (cons 0 ub-risks)))
;; ((0 0.0005456) (0.0005456 0.0009988) ... (0.1017762 0.6340773)

(defn uniform-sample-on "uniform sample on the interval [lb, ub]"
  [rng [lb ub]] (+ lb (* (rand01 rng) (- ub lb))))

(defn- normal
  "Transform a sequence urs of uniform random number in the interval [0, 1)
   into a sequence of normal random numbers with mean mu and standard
   deviation sigma."
  [rng mu sigma]
  ; This function implements the Kinderman-Monahan ratio method:
  ;  A.J. Kinderman & J.F. Monahan
  ;  Computer Generation of Random Variables Using the Ratio of Uniform Deviates
  ;  ACM Transactions on Mathematical Software 3(3) 257-260, 1977
  (loop []
    (let [u1 (rand01 rng)
          u2* (rand01 rng)
          u2 (- 1. u2*)
          s (* 4 (/ (Math.exp (- 0.5)) (Math.sqrt 2.)))
          z (* s (/ (- u1 0.5) u2))
          zz (+ (* 0.25 z z) (Math.log u2))]
      (if (> zz 0)
        (recur)
        (+ mu (* sigma z))))))

(defn to-survival-% "convert p(death) to % chance of survival "
  [risk] (* 100 (- 1 risk)))

(defn random-choice "choose one item randomly from a collection"
  ;;todo: make this use rng instead of sample.
  [rng coll]
  (nth coll (random/next-int! rng (count coll))))


(defn sampled "take n full precision numeric samples"
  [rng n]
  (map #(to-survival-%
         (uniform-sample-on rng
                            (random-choice rng centiles))) (range n)))

(defn risk-category "annotate a risk value with category and icon"
  [rng rate] (cond
               (< rate 90) {:risk :high
                            :icon (random-choice rng [:child-bed :incubator :toddler-cot :baby-cot])}
               (<= rate 99) {:risk :medium
                             :icon (random-choice rng [:toddler :toddler-cot :baby-cot :yboy :ygirl])}
               (> rate 99) {:risk :low
                            :icon (random-choice rng [:toddler :yboy :oboy :ygirl :ogirl])}))

(defn linear
  "apply a linear transform to a value"
  [[m c] value]
  (+ (* m value) c))

(defn decorated "decorate n samples with string representation, icon, and risk band"
  [rng n]
  (map #(let [{:keys [risk icon]} (risk-category rng %)]
         {:rate      %
          :formatted (.toFixed (js/Number. %) 0)
          :icon      icon
          :risk      risk}) (sampled rng n)))

(defn simple-frequencies
  "Simpler - and in this case, faster - algorithm for buckets of size 1"
  [values]
  (into (sorted-map) (frequencies (map Math/ceil values))))

(defn sample-count
  "Returns the number of observed values in a frequency map."
  [freq-map]
  (reduce + (vals freq-map)))

(defn quantile*
  "Like quantile but takes sample-count as an argument. For when you
  already know the sample-count and don't want to recompute it. Also
  assumes that the frequency map is already sorted."
  [sorted-freq-map k q sample-count]
  (let [rank (long (Math/ceil (* k (/ (double sample-count) q))))]
    (loop [m (seq sorted-freq-map)
           lower 0
           prev-value js/Number.NEGATIVE_INFINITY
           ]
      (if-let [entry (first m)]
        (let [[value freq] entry
              upper (+ lower freq)]
          (if (<= rank upper)
            value
            (recur (rest m) upper value)))
        prev-value))))

(defn- ensure-sorted [m]
  (if (sorted? m)
    m
    (into (sorted-map) m)))

(defn quantile
  "Returns the value which is greater than k/q of the observed values
  in the frequency map. For example, k=1 q=2 is the median; k=99 q=100
  is the 99th percentile. For bucketed frequency maps, returns the
  nearest bucket."
  [freq-map k q]
  (quantile* (ensure-sorted freq-map) k q (sample-count freq-map)))