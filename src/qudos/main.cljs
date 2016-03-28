(ns ^:figwheel-always qudos.main
  (:require [rum.core :as rum]
            [qudos.calc :as calc]
            [clojure.string :as s]
            ))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(rum/defc
  icon-block []
  [:div
   (for [p (partition 20 (calc/sampled 100))]
     [:p (s/join " " p)])]
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

