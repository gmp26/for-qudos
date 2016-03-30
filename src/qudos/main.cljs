(ns ^:figwheel-always qudos.main
  (:require [rum.core :as rum]
            [qudos.calc :as calc]
            [clojure.string :as s]
            ))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(def background
  {:low "#F8F8E8"
   :normal "#E0E8E8"
   :high "#DECFCC"})

(rum/defc
  icon-block []
  [:div {:style {:padding "30px"}}
   (for [row (partition 10 (calc/decorated 100))]
     [:div {:style {:zoom    0.2}}
      (for [item row]
        [:div {:style {:display          "inline-table"
                       :margin           "5px"
                       :padding          "35px 60px 40px 5px"
                       :border-radius    "30px"
                       :background-color ((:risk item) background)
                       }}
         [:img {:src (str "assets/" (name (:icon item)) ".png")}]])
      ])]
  )

(rum/defc
  root []
  [:div
   [:h1 "Root element"]
   (icon-block)])

(defn el [id] (js/document.getElementById id))


(rum/mount (root) (el "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

