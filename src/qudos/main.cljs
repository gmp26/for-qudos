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

(defonce simulation (atom {:decorated    (calc/decorated @sample-rng 100)
                           :future-count 0
                           :future-seed  0
                           :m            1
                           :c            0}))

(defn survival-count
  "find the deaths for a given future index"
  [index decorated-sample]
  (let [sim @simulation
        rng (random/create (+ index (:future-seed sim)))]
    (- 100 (Math.round (calc/normal rng (:m sim) (:c sim))))
    #_(count (filter #(>= (:rate %) (calc/rand-n rng 100)) decorated-sample))))

(defn resample
  [n]
  (let [sim @simulation]
    (reset! sample-seed n)
    (reset! sample-rng (random/create @sample-seed))
    (swap! simulation assoc :decorated (calc/decorated @sample-rng 100))))

(defn rescale
  [m c]
  (swap! simulation assoc :m m :c c)
  )

(defn reseed
  [n]
  (reset! seed n)
  (reset! rng (random/create @seed)))

(resample 0)
(rescale 1 0)
(reseed 1)

(defn death-mask
  "show mask for deaths for a given future index"
  [index decorated-sample mask]
  (let [sim @simulation
        rng (random/create (dec (+ index (:future-seed sim))))]
    (js/setTimeout #(resample @sample-seed) 200)
    (let [deaths (Math.round (calc/normal rng (:m sim) (:c sim)))]
      (map-indexed #(if (< %1 deaths) (assoc %2 :risk mask) %2) decorated-sample))
    #_(map #(if (< (:rate %) (calc/rand-n rng 100)) (assoc % :risk mask) %) decorated-sample)))

(rum/defc
  icon-block < rum/reactive  []

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
    (swap! simulation assoc :decorated (death-mask (:future-count sim) (:decorated sim) mask))))

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
          (let [sim (rum/react simulation)]
            [:.container
             [:h2 "100 operations"]
             [:.row
              [:.col-md-7
               (icon-block)
               [:div {:style {:margin-top " 20px"}}]
               (hist (map-indexed #(survival-count %1 (:decorated (rum/react simulation)))
                                  (range (:future-count (rum/react simulation)))))]
              ;(map #(calc/survival-count @rng (:decorated (rum/react simulation))) (range 500)))

              [:form.form-horizontal.col-md-5 {:style {:border "1px solid #CCCCCC"}}
               [:.form-group
                [:h3.col-md-12 "Controls"]]

               [:.form-group
                [:label.col-md-3.text-right {:for "sample"} " sample: "]
                [:input#sample.col-md-5 {:style     {:width "90px"
                                                     :zoom  1
                                                     :margin-right "20px"}
                                         :type      "number"
                                         :value     (rum/react sample-seed)
                                         :on-change #(resample (js/parseInt (.. % -target -value)))}]]

               [:.form-group
                [:label.col-md-3.text-right {:for "mu"} [:i " mu"]]
                [:input#m.col-md-5 {:style     {:width "90px"
                                                :zoom  1}
                                    :type      "number"
                                    :value     (:m sim)
                                    :on-change #(rescale (js/parseFloat (.. % -target -value)) (:c sim))}]

                [:label.col-md-3.text-right {:for "c"} [:i " sigma"]]
                [:input#c.col-md-5 {:style     {:width "90px"
                                                :zoom  1}
                                    :type      "number"
                                    :value     (:c sim)
                                    :on-change #(rescale (:m sim) (js/parseFloat (.. % -target -value)))}]]

               [:hr]
               [:.form-group
                [:label.col-md-7.text-right {:for "pr"} " predicted range: "]

                ;; choose 0/mu=4.5/sigma=0.7/futures=60/future-seed=76 yay!
                [:label#pr.col-md-5 94 "% - " 97 "%"]

                ;; choose 0/mu=3.5/sigma=1.1/futures=60/future-seed=126
                [:label.col-md-7.text-right {:for "epr"} " extended predicted range: "]
                [:label#epr.col-md-5 95 "% - " 99 "%"]]

               [:.form-group
                [:label.col-md-7.text-right {:for "fcount"} " futures: "]
                [:input#fcount.col-md-5 {:style     {:width "90px"
                                                     :zoom  1}
                                         :type      "number"
                                         :value     (:future-count (rum/react simulation))
                                         :on-change #(do
                                                      (swap! simulation assoc
                                                             :future-count (js/parseInt (.. % -target -value)))
                                                      (show-deaths :dead))}]]
               [:.form-group
                [:label.col-md-7.text-right {:for "fseed"} " future-seed: "]
                [:input#fseed.col-md-5 {:style     {:width "90px"
                                                    :zoom  1}
                                        :type      "number"
                                        :value     (:future-seed (rum/react simulation))
                                        :on-change #(swap! simulation assoc
                                                           :future-seed (js/parseInt (.. % -target -value)))}]]]]]))

(defn el [id] (js/document.getElementById id))

(rum/mount (root) (el "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

