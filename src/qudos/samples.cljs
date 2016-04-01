(ns ^:figwheel-always qudos.samples
  (:require
    [qudos.calc :as calc]
    [clojure.string :as s]))

(defonce simulation
         (atom [{:rng-seed         3
                 :sample-seed      42
                 :generated-sample []
                 :generated-deaths []}]))


(defn run-simulation
  [] "run simulation with many seeds, outputting data that features in the animation. This is to choose the best seeds"
  (let [rng-seed 0
        sample-seed 1
        result []]
    (conj result {:generated (calc/decorated 100 sample-seed)})))



(def targets
  {:slide-10 {:hospital-1 {:n 60 :deaths 2 :pr-lb 94 :pr-ub 97 :observed 96}
              :hospital-2 {:n 40 :deaths 1 :pr-lb 95 :pr-ub 99 :observed 98}}
   :slide-11 {:n 100 :pr-lb 96 :pr-ub 99 :epr-lb 94 :epr-ub 100}

})