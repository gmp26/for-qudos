(ns ^:figwheel-always qudos.main
  (:require [rum.core :as rum]
            [qudos.calc :as calc]
            [clojure.string :as s]
            [b1.charts :as c]
            [b1.svg :as svg]
            [b1.ticks :as ticks :refer [search]]
            [b1.layout.histogram :as h]
            [b1.maths :refer [Pi Tau radians-per-degree
                              sin cos mean]]
            [b1.scale :as scale]


            ))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(rum/defc
  icon-block []
  [:div {:style {:padding "30px"}}
   (for [row (partition 20 (calc/decorated 100 3))]
     [:div {:style {:zoom    0.2}}
      (for [item row]
        [:div {:style {:display          "inline-table"
                       :background-image (str "url(assets/" (name (:risk item)) "-risk.png)")
                       }}
         [:img {:src (str "assets/" (name (:icon item)) ".png")}]])])])


(def data (take 100 (repeatedly #(+ 90 (* 10 (rand))))))
(def more-data (take 100 (repeatedly #(+ 90 (* 10 (rand))))))
;(def more-data (take 100 (repeatedly #(* 100 (rand)))))

(defmulti as-svg (fn [chart & args] (:type chart)))
(defmethod as-svg :histogram [{:keys [histograms  bins x-axis y-axis]
                               :or {x-axis true y-axis true}} & {:keys [width height]}]
  (let [margin-vertical (if x-axis 40 0)
        margin-horizontal (if y-axis 40 0)
        bars (->> histograms
                  (map-indexed (fn [i xs]
                                 (map #(assoc % :series i) xs)))
                  (apply concat))
        bar-width (/ (- width (* 2 margin-horizontal)) (count bars))
        scale-x (scale/linear :domain x-axis
                              :range [margin-horizontal
                                      (- width margin-horizontal)])
        max-y (->> bars
                   (map :y)
                   (apply max))
        scale-y (scale/linear :domain [0 max-y]
                              :range [(- height margin-vertical)
                                      margin-vertical])]
    ;(prn (map #(:series %) bars))
    [:svg {:width width :height height}
     [:g {:transform (svg/translate [margin-horizontal 0])}
      (svg/axis scale-y (:ticks (search [0 max-y])) :orientation :left)]
     [:g {:transform (svg/translate [0 (- height margin-vertical)])}
      (svg/axis scale-x
                ;(:ticks {:extent [90 100], :min 90, :max 100, :ticks [90 92 94 96 98 100]})
                (:ticks (search x-axis))
                :orientation :bottom)]
     [:g.chart
      (for [bar bars]
        [:g.bar {:class (str "series-" (:series bar))
                 :transform (svg/translate [(+ (-> bar :x scale-x)
                                           (* bar-width (:series bar) 0.5))
                                        (-> bar :y scale-y)])}
         [:rect {:height (->> bar :y scale-y (- height margin-vertical))
                 :width (/ bar-width 2)}]])]]))

(def hist
  (-> (c/histogram data :x-axis [0 100] :bins 10)
      (c/add-histogram more-data)
      (svg/as-svg :width 500 :height 200)))

(rum/defc
  root []
  [:div
   [:h1 "Root element"]
   (icon-block)
   hist
   ])

(defn el [id] (js/document.getElementById id))

(rum/mount (root) (el "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

