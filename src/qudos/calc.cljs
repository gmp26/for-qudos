(ns ^:figwheel-always qudos.calc
  (:require [rum.core :as rum]
            [clojure.string :as s]
            [cljs.pprint :as pp]
            [cljx-sampling.random :as random]
            [cljx-sampling.core :refer [sample]]))

(enable-console-print!)
;; See https://github.com/ashenfad/cljx-sampling

(def rng (random/create 42))

(defn rand01 [] (random/next-double! rng 1))

(def ub-risks [0.0005456 0.0009988 0.0015798 0.0023618 0.0033583
               0.0044388 0.0056236 0.0068915 0.0084389 0.0102027
               0.0123412 0.0151487 0.0184223 0.0226536 0.0278246
               0.0348422 0.0463867 0.0640427 0.1017762 0.6340773])

(def centiles (partition 2 1 (cons 0 ub-risks)))
;; ((0 0.0005456) (0.0005456 0.0009988) ... )

(defn uniform-sample-on "uniform sample on the interval [lb, ub]"
  [[lb ub]] (+ lb (* (rand01) (- ub lb))))

(defn to-survival-% "convert p(death) to % chance of survival "
  [risk] (* 100 (- 1 risk)))

(defn sampled "take n full precision numeric samples"
  [n] (->> centiles
           (#(sample % :replace true :seed 0))
           (map uniform-sample-on)
           (map to-survival-%)
           (take n)))

(defn random-choice "choose one item randomly from a collection"
  ;;todo: make this use rng instead of sample.
  [coll] (first (take 1 (sample coll :replace true))))

(defn risk-category "annotate a risk value with category and icon"
  [rate] (cond
           (< rate 90) {:risk :high
                        :icon (random-choice [:baby :incubator :toddler-bed])}
           (<= rate 99) {:risk :normal
                         :icon (random-choice [:baby :toddler :yboy :ygirl])}
           (> rate 99) {:risk :low
                        :icon (random-choice [:toddler :yboy :oboy :ygirl :ogirl])}))

(defn decorated "decorate n samples with string representation, icon, and risk band"
  [n] (->> (sampled n)
           (map #(let [{:keys [risk icon]} (risk-category %)]
                  {:rate      %
                   :formatted (.toFixed (js/Number. %) 0)
                   :icon      icon
                   :risk      risk}))))

;; (defn a-possible-future [n]
;;  (apply str (take n (repeatedly #(if (< (rand) (sim-risk)) "   " ":) ")))))

