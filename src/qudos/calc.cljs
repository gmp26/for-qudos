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
      (if (pos? zz)
        (recur)
        (+ mu (* sigma z))))))

(defn- skew-normal
  "Generate a random variable z from the skew normal distribution with shape parameter delta.
  Return az+b. See http://azzalini.stat.unipd.it/SN/faq-r.html"
  [rng mu sigma delta]
  (let [u0 (normal rng 0 1)
        v (normal rng 0 1)
        u1 (+ (* delta u0) (* v (Math.sqrt (- 1 (* delta delta)))))
        z (if (>= u0 0) u1 (- u1))]
    (+ mu (* sigma z))))


;;;
;;
;; Calculating skew normal quantiles - table from the R sn package.
;;
;;;
;; for(i in seq(-1,1,0.1))
;;   {print (qsn (c (0.001, 0.025, 0.975, 0.999), alpha=i / (1-i*i))) ;}

(def sn-quantiles [[-3.090232 -1.959964 1.959964 3.090232]
                   [-2.999788 -1.873362 2.033945 3.160795]
                   [-2.883772 -1.770220 2.097484 3.214648]
                   [-2.733242 -1.644706 2.151096 3.253169]
                   [-2.535313 -1.488983 2.193685 3.276984]
                   [-2.273243 -1.293831 2.223017 3.287806]
                   [-1.929257 -1.051269 2.237714 3.290375]
                   [-1.493351 -0.761077 2.241257 3.290526]
                   [-0.9792991 -0.4413550 2.2414027 3.2905267]
                   [-0.4398280 -0.1387332 2.2414027 3.2905267]
                   [0.001253314 0.031337982 2.241402728 3.290526731]])

(defn linear
  "apply a linear transform ax+b to value x"
  [[a b] value]
  (+ (* a value) b))

(defn lerp-v
  "linear interpolation of vector y given x where (x,y) is on (x0,y0) (x1,y1)"
  [x0 y0 x1 y1 x]
  (map + y0 (map / (map * (map - y1 y0) (repeat (- x x0))) (repeat (- x1 x0)))))

(defn predicted-range
  " -1 < delta < 1, and we tabulate at 0.1 intervals. Interpolate the predicted range"
  [deaths spread delta]
  (prn delta)
  (let [ix-delta (max 0 (min 10 (* (Math.abs delta) 10)))
        x0 (Math.floor ix-delta)
        x1 (Math.ceil ix-delta)
        y (if (= x0 x1)
          (sn-quantiles x0)
          (lerp-v x0 (sn-quantiles x0) x1 (sn-quantiles x1) ix-delta))]
    (prn delta x0 x1 y)
    (vec (map #(linear [spread deaths] %)
              (if (neg? delta) y (map - (reverse y)))))))

(defn to-survival-% "convert p(death) to % chance of survival "
  [risk] (* 100 (- 1 risk)))

(defn random-choice "choose one item randomly from a collection"
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
                            :icon (random-choice rng [:inc :inc :inc :inc :inc :inc :cot :cot :cot :cot :cot :cot :bed :bed :bed :bed :bed :bed :yboy1 :ygirl1])}
               (<= rate 99) {:risk :medium
                             :icon (random-choice rng [:inc :bed :bed :cot :cot :cot :yboy1 :ygirl1 :yboy2 :ygirl2 :yboy1 :ygirl1])}
               (> rate 99) {:risk :low
                            :icon (random-choice rng [:inc :cot :bed :yboy1 :yboy2 :ygirl1 :ygirl2 :ogirl1 :ogirl2 :oboy1 :oboy2 ])}))

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