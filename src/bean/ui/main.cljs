(ns bean.ui.main
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.sheet :as sheet]
            [bean.ui.views :as views]
            [bean.ui.events :as events]
            [re-frame.core :as rf]
            [reagent.dom :as r]))

(defn ^:dev/after-load ^:export main []
  (rf/dispatch-sync [::events/initialize-db])
  (r/render
   [views/main-panel]
   (.getElementById js/document "app")))
