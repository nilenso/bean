(ns bean.ui.main
  (:require [bean.ui.views.root :as root]
            [bean.ui.events :as events]
            [bean.ui.routes :as routes]
            [re-frame.core :as rf]
            [reagent.dom :as r]))

(defn ^:dev/after-load ^:export main []
  (rf/dispatch-sync [::events/initialize-db])
  (routes/start)
  (r/render
   [root/routed]
   (.getElementById js/document "app")))
