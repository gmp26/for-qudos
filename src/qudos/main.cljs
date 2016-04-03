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

(defonce seed (atom 0))
(defonce rng (atom (random/create @seed)))
(defonce simulation (atom {:rng       @rng
                           :decorated (calc/decorated @rng 100)}))

(defn reseed
  ([]
   (reset! rng (random/create @seed))
   (swap! simulation assoc :rng @rng :decorated (calc/decorated @rng 100)))

  ([n]
   (reset! seed n)
   (reseed)))

(reseed)

(rum/defc
  icon-block < rum/reactive []
  [:div {:style {:padding "30px"}}
   (for [row (partition 20 (:decorated (rum/react simulation)))]
     [:div {:style {:zoom 0.1}}
      (for [item row]
        [:div {:style {:display          "inline-table"
                       :background-image (str "url(assets/" (name (:risk item)) "-risk.png)")
                       }}
         [:img {:src (str "assets/" (name (:icon item)) ".png")}]])])])

(def data (take 60 (repeatedly #(+ 90 (* 11 (rand))))))

(def more-data (take 100 (repeatedly #(+ 90 (* 11 (rand))))))

(defn hist
  [rates1 & [rates2]]
  (svg/as-svg
    (if rates2
      (c/add-histogram (c/tallies rates1 :x-axis [90 101] :y-axis false :bins 11 :tally-h 0.5)
                       rates2)
      (c/tallies rates1 :x-axis [90 101] :y-axis false :bins 11 :tally-h 6))
    :width 300 :height 200)
  )

(defn prev-seed
  [event] (prn "next"))

(defn next-seed
  [event] (swap! seed inc))

(defn survivors []
  (calc/survival-count (:rng @simulation)
                       (calc/decorated (:rng @simulation) 100)))

(rum/defc root < rum/reactive
          [rates]
          [:div
           [:h1 "Root element"]
           (icon-block)
           [:span {:style {:margin-left "40px"}}
            (hist (map survivors (range 60))
                  (map survivors (range 200))
                  )
            [:div {:style {:margin-left "40px"}}
             "seed: "
             [:button.btn.btn-primary {:on-click #(swap! seed dec)}
              "-"]
             [:input {:style     {:width " 60px"}
                      :type      "number"
                      :value     (rum/react seed)
                      :on-change #(reseed (.parseInt js/Number. (.. % -target -value)))}]
             ]]
           ])

(defn el [id] (js/document.getElementById id))

(rum/mount (root) (el "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

