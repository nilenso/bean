(ns bean.ui.main
  (:require [bean.ui.views.root :as root]
            [bean.ui.events :as events]
            [re-frame.core :as rf]
            [reagent.dom :as r]))

(defn ^:dev/after-load ^:export main []
  (rf/dispatch-sync [::events/initialize-db])
  (r/render
<<<<<<< HEAD
   [views/main-panel]
||||||| parent of 23ae4f5 (Move views out to a views directory)
  ;;  TODO: why extract only the main panel to views
   [views/main-panel]
=======
   [root/root-page]
>>>>>>> 23ae4f5 (Move views out to a views directory)
   (.getElementById js/document "app")))
