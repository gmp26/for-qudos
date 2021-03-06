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
                           :future-seed  10
                           :m            4.5
                           :c            0.7
                           :skew         0
                           :deaths-visible true
                           :backgrounds-visible true}))

(defn survival-count
  "find the deaths for a given future index"
  [index]
  (let [sim @simulation
        rng (random/create (+ index (:future-seed sim)))]
    (- 100 (Math.round (calc/skew-normal rng (:m sim) (:c sim) (:skew sim))
                       #_(if (zero? (:skew sim))
                         (calc/normal rng (:m sim) (:c sim))
                         (calc/skew-normal rng (:m sim) (:c sim) (:skew sim)))))))

(defn resample
  [n]
  (let [sim @simulation]
    (reset! sample-seed n)
    (reset! sample-rng (random/create @sample-seed))
    (swap! simulation assoc :survivors nil :decorated (calc/decorated @sample-rng 100))))

(defn rescale
  ([m c skew]
   (swap! simulation assoc :m m :c c :skew skew))
  ([m c]
   (rescale m c 0)))

(defn reseed
  [n]
  (reset! seed n)
  (reset! rng (random/create @seed)))

(resample 0)
(rescale 4.5 0.7)
(reseed 10)

(def show-death-time 0)                                     ; positive millisecs or forever

(defn death-mask
  "apply mask for deaths at a given future frame index"
  [index decorated-sample mask]
  (let [sim @simulation
        rng (random/create (+ index (:future-seed sim)))
        ;rng (random/create (dec (+ index (:future-seed sim))))
        ]
    (if (pos? show-death-time)
      (js/setTimeout #(resample @sample-seed) show-death-time))

    (let [deaths (- 100 (survival-count index))
          ;;(Math.round (calc/normal rng (:m sim) (:c sim)))
          to-mask (into #{} (map first (take deaths
                                             (sort-by second <
                                                      (map-indexed
                                                        (fn [i v] [i (+ (calc/normal rng 0 30) (:rate v))])
                                                        decorated-sample)))))]
      (swap! simulation assoc :survivors (- 100 deaths))
      ;(prn (str "simulation = " (:survivors @simulation)))
      ;(prn (str "to-mask = " to-mask))


      (map-indexed #(if (to-mask %1)
                     (assoc %2 :dead mask) %2) decorated-sample))))


(defn show-deaths
  "show/hide deaths in the icon array for the current future"
  []
  (let [sim @simulation]
    (swap! simulation assoc :decorated (death-mask (:future-count sim) (:decorated sim) :dead))))

(defn show-frame
  "move model forward to the given frame and update the view"
  [frame-number]
  (swap! simulation assoc :future-count frame-number)
  (resample @sample-seed)
  (show-deaths))

(defn control-simulation
  "Package all simulation control parameters into one function (for frame-by-frame views in devcards)"
  [& {:keys [sample-seed future-seed frame deaths spread skew]
      :or   {sample-seed 20 future-seed 30 frame 0 deaths 4 spread 0.7 skew 0}}]
  (resample sample-seed)
  (reseed future-seed)
  (rescale deaths spread skew)
  (show-frame frame))

#_(defn predicted-ranges
  "Take many samples and estimate predicted and extended predicted ranges"
  [& {:keys [sample-seed future-seed frame deaths spread skew]
      :or   {sample-seed 20 future-seed 30 frame 0 deaths 4 spread 0.7 skew 0}}])

(defn play []
  ;(prn "play")
  (show-frame (inc (:future-count @simulation)))
  (when (< (:future-count @simulation) 20)
    (js/setTimeout play 500)))

(rum/defc
  icon-block < rum/reactive []
  (let [sim (rum/react simulation)]
    [:div
     (for [row (partition 10 (:decorated sim))]
       [:div {:style {:zoom 0.1}}
        (for [item row]
          [:div {:style {:display           "inline-table"
                         :padding           "0px"
                         :background-repeat "no-repeat"
                         :background-image  (if (:backgrounds-visible sim)
                                              (str "url(assets/" (name (:risk item)) "-risk.png)")
                                              "none")
                         :background-size   "400px"
                         }}

           [:img {:style {:width "400px" :height "400px"}
                  :src   (str "assets/"
                              (if (and (= (:dead item) :dead) (:deaths-visible sim))
                                "dead-risk"
                                (name (:icon item)))
                              ".png")}]])])]))

(defn hist
  "draw a tally chart"
  [rates1 & [rates2]]
  (svg/as-svg
    (if rates2
      (c/add-histogram (c/tallies rates1 :x-axis [90 101] :y-axis false :bins 11 :tally-h 0.5)
                       rates2)
      (c/tallies rates1 :x-axis [90 101] :y-axis false :bins 11 :tally-h 6))
    :width 300 :height 360))

(rum/defc show-survival-percent < rum/reactive []
          (let [survivors (:survivors (rum/react simulation))]
            [:div {:style {:font-size    "36px"
                           :padding-left "0px"
                           :height       "45px"}}
             (if (some? survivors)
               (str (.toFixed survivors 0 (js/Number.)) "% survival") "")]))

(rum/defc root < rum/reactive
          [rates]
          (let [sim (rum/react simulation)
                pr (vec (map #(.toFixed % 1 (js/Number.))
                             (calc/predicted-range (- 100 (:m sim)) (:c sim) (:skew sim))))]
            [:.container
             [:.row
              [:h2 (str "100 operations")]
              ;[:p (str "show deaths: " (:deaths-visible sim))]
              ;[:p (str @simulation)]
              ]

             [:.row
              [:.col-md-7
               (icon-block)
               [:div {:style {:margin-top " 20px"}}]
               ]

              ;; out by one error fixed by inc in next statement.

              ;;(map #(calc/survival-count @rng (:decorated (rum/react simulation))) (range 500)))

              [:.col-md-5 {:style {:margin-top "0px" :padding-left "23px"}}
               (show-survival-percent)
               (hist (map-indexed #(survival-count (inc %1))
                                  (range (:future-count (rum/react simulation)))))]]
             [:.row
              [:form.form-horizontal.col-md-6 {:style {:border "1px solid #CCCCCC"}}
               [:.form-group
                [:h3.col-md-12 "Controls"]]

               [:.form-group
                [:label.col-md-3.text-right {:for "sample"} " sample: "]
                [:input#sample.col-md-3 {:style     {:width "90px"
                                                     :zoom  1
                                                     }
                                         :type      "number"
                                         :value     (rum/react sample-seed)
                                         :on-change #(resample (js/parseInt (.. % -target -value)))}]
                [:label.col-md-3.text-right {:for "mu"} [:i " deaths"]]
                [:input#m.col-md-3 {:style     {:width "90px"
                                                :zoom  1}
                                    :type      "number"
                                    :value     (:m sim)
                                    :on-change #(rescale (js/parseFloat (.. % -target -value)) (:c sim) (:skew sim))}]]

               [:.form-group
                [:label.col-md-3.text-right {:for "skew"} [:i " skew %"]]
                [:input#m.col-md-5 {:style     {:width "90px"
                                                :zoom  1}
                                    :type      "number"
                                    :min       -100
                                    :max       100
                                    :value     (Math.round (* 100 (:skew sim)))
                                    :on-change #(rescale (:m sim) (:c sim) (/ (js/parseInt (.. % -target -value)) 100))}]

                [:label.col-md-3.text-right {:for "c"} [:i " spread"]]
                [:input#c.col-md-5 {:style     {:width "90px"
                                                :zoom  1}
                                    :type      "number"
                                    :value     (:c sim)
                                    :on-change #(rescale (:m sim) (js/parseFloat (.. % -target -value)) (:skew sim))}]]

               [:hr]
               [:.form-group
                [:label.col-md-7.text-right {:for "pr"} " predicted range: "]

                ;; choose 0/mu=4.5/sigma=0.7/futures=60/future-seed=76 yay!
                [:label#pr.col-md-5 (min 100 (pr 1)) "% - " (min 100 (pr 2)) "%"]

                ;; choose 0/mu=3.5/sigma=1.1/futures=60/future-seed=126
                [:label.col-md-7.text-right {:for "epr"} " extended predicted range: "]
                [:label#epr.col-md-5 (min 100 (pr 0)) "% - " (min 100 (pr 3)) "%"]]

               [:.form-group
                [:label.col-md-7.text-right {:for "fcount"} " frame: "]
                [:input#fcount.col-md-3 {:style     {:width "90px"
                                                     :zoom  1}
                                         :min       0
                                         :type      "number"
                                         :value     (:future-count (rum/react simulation))
                                         :on-change #(show-frame (js/parseInt (.. % -target -value)))
                                         }]
                [:button.btn-primary {:on-click play
                                      :type     "button"} [:i.fa.fa-play]]]

               [:.form-group
                [:label.col-md-7.text-right {:for "fseed"} " future-seed: "]
                [:input#fseed.col-md-5 {:style     {:width "90px"
                                                    :zoom  1}
                                        :type      "number"
                                        :value     (:future-seed (rum/react simulation))
                                        :on-change #(swap! simulation assoc
                                                           :future-seed (js/parseInt (.. % -target -value)))}]]

               [:.form-group

                [:.checkbox
                 [:label
                  [:input#show-deaths.col-md-8 {:type      "checkbox"
                                                :checked   (:deaths-visible (rum/react simulation))
                                                :on-change #(do
                                                             (.log js/console %)
                                                             (show-deaths)
                                                             (swap! simulation assoc
                                                                    :deaths-visible (.. % -target -checked)))}]
                  "show deaths"]]
                [:.checkbox
                 [:label
                  (prn "backgrounds-visible: " (:backgrounds-visible (rum/react simulation)))
                  [:input#show-deaths.col-md-8 {:type      "checkbox"
                                                :checked   (:backgrounds-visible (rum/react simulation))
                                                :on-change #(do
                                                             (.log js/console %)
                                                             (swap! simulation assoc
                                                                    :backgrounds-visible (.. % -target -checked)))}]
                  "show risk backgrounds"]]
                ]]]]))

(defn el [id] (js/document.getElementById id))

(rum/mount (root) (el "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

