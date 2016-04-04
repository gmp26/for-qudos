(ns ^:figwheel-always qudos.main
  (:require [rum.core :as rum]
            [cljx-sampling.random :as random]
            [qudos.calc :as calc]
            [clojure.string :as s]
            [b1.charts :as c]
            [b1.svg :as svg]
            [b1.layout.histogram :as h]
            [b1.maths :refer [Pi Tau radians-per-degree
                              sin cos mean]]
            [b1.scale :as scale]
            ))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce sample-seed (atom 1))
(defonce sample-rng (atom (random/create @sample-seed)))

(defonce seed (atom 0))
(defonce rng (atom (random/create @seed)))

(defonce simulation (atom {:decorated    (calc/decorated @sample-rng 100) :decorated-death nil
                           :future-count 0
                           :future-seed  0}))

#_(defn survival-count "calculate a random mask for a sample of n operations, where true means dead"
    [rng decorated-sample]
    (count (filter #(>= (:rate %) (calc/rand-n rng 100)) decorated-sample)))

(defn survival-count
  "find the deaths for a given future index"
  [index decorated-sample]
  (let [sim @simulation
        rng (random/create (+ index (:future-seed sim)))]
    (count (filter #(>= (:rate %) (calc/rand-n rng 100)) decorated-sample))))



(defn resample
  [n]
  (reset! sample-seed n)
  (reset! sample-rng (random/create @sample-seed))
  (swap! simulation assoc :decorated (calc/decorated @sample-rng 100)))

(defn reseed
  [n]
  (reset! seed n)
  (reset! rng (random/create @seed)))

(resample 0)
(reseed 1)

(defn death-mask
  "show mask for deaths for a given future index"
  [index decorated-sample mask]
  (let [sim @simulation
        rng (random/create (dec (+ index (:future-seed sim))))]
    (js/setTimeout #(resample @sample-seed) 200)
    (map #(if (< (:rate %) (calc/rand-n rng 100)) (assoc % :risk mask) %) decorated-sample)
    )
  )


(rum/defc
  icon-block < rum/reactive []
  [:div
   (for [row (partition 20 (:decorated (rum/react simulation)))]
     [:div {:style {:zoom 0.1}}
      (for [item row]
        [:div {:style {:display          "inline-table"
                       :background-image (str "url(assets/" (name (:risk item)) "-risk.png)")
                       }}
         [:img {:src (str "assets/" (if (= (:risk item) :dead)
                                      "fill-" "")
                          (name (:icon item)) ".png")}]])])])


(defn show-deaths
  "show/hide deaths in the icon array for the current future"
  [mask]
  (let [sim @simulation]
    (swap! simulation assoc :decorated (death-mask (:future-count sim) (:decorated sim) mask)))
  )

#_(def data (take 60 (repeatedly #(+ 90 (* 11 (rand))))))

#_(def more-data (take 100 (repeatedly #(+ 90 (* 11 (rand))))))

(defn hist
  [rates1 & [rates2]]
  (svg/as-svg
    (if rates2
      (c/add-histogram (c/tallies rates1 :x-axis [90 101] :y-axis false :bins 11 :tally-h 0.5)
                       rates2)
      (c/tallies rates1 :x-axis [90 101] :y-axis false :bins 11 :tally-h 6))
    :width 250 :height 300))


(rum/defc root < rum/reactive
          [rates]
          [:.container
           [:h1 "Icon array"]
           [:.row
            [:.col-md-7
             (icon-block)
             [:div {:style {:margin-top " 20px"}}]
             (hist (map-indexed #(survival-count %1 (:decorated (rum/react simulation))) (range (:future-count (rum/react simulation)))))]
            ;(map #(calc/survival-count @rng (:decorated (rum/react simulation))) (range 500)))

            [:form.form-horizontal.col-md-5 {:style {:border "1px solid #CCCCCC"}}
             [:.form-group
              [:h3.col-md-12 "Controls"]]
             [:.form-group
              [:label.col-md-4{:for "sample"} " sample: "]
              [:input#sample.col-md-8 {:style     {:width "90px"
                                                   :zoom  1.5}
                                       :type      "number"
                                       :value     (rum/react sample-seed)
                                       :on-change #(resample (.parseInt js/Number. (.. % -target -value)))}]]

             #_[:.form-group
                [:label.col-md-4 {:for "seed"} " seed: "]
                [:input#seed.col-md-8 {:style     {:width "90px"
                                                   :zoom  1.5}
                                       :type      "number"
                                       :value     (rum/react seed)
                                       :on-change #(reseed (.parseInt js/Number. (.. % -target -value)))}]]

             [:.form-group
              [:label.col-md-4 {:for "fcount"} " futures: "]
              [:input#fcount.col-md-10 {:style     {:width "90px"
                                                    :zoom  1.5}
                                        :type      "number"
                                        :value     (:future-count (rum/react simulation))
                                        :on-change #(do
                                                     (swap! simulation assoc :future-count (.parseInt js/Number. (.. % -target -value)))
                                                     (show-deaths :dead))}]]
             [:.form-group
              [:label.col-md-4 {:for "fseed"} " future-seed: "]
              [:input#fseed.col-md-10 {:style     {:width "90px"
                                                   :zoom  1.5}
                                       :type      "number"
                                       :value     (:future-seed (rum/react simulation))
                                       :on-change #(swap! simulation assoc :future-seed (.parseInt js/Number. (.. % -target -value)))}]]
             ]]

           ])

(defn el [id] (js/document.getElementById id))

(rum/mount (root) (el "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

