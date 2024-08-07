(ns bean.ui.main
  (:require [bean.ui.events :as events]
            [bean.ui.views.sheet :as sheet]
            [bean.ui.routes :as routes]
            [bean.ui.views.root :as root]
            [bean.ui.save :as save]
            [re-frame.core :as rf]
            [reagent.dom :as r]))

(defn ^:dev/after-load main* []
  (routes/start)
  (r/render
   [root/routed]
   (.getElementById js/document "app"))
  (rf/dispatch [::events/reload-bindings]))

(defn init []
  (.addEventListener js/window "keydown" (fn [e] (sheet/handle-global-kbd e)))
  (.addEventListener js/window "paste" (fn [e] (sheet/handle-paste e)))
  (.addEventListener js/window "copy" (fn [e] (sheet/handle-copy e)))
  (.addEventListener js/window "cut" (fn [e] (sheet/handle-cut e))))

(defn ^:export main []
  (-> (save/read-sheet)
      (.then #(do (rf/dispatch-sync [::events/initialize-db %])
                  (rf/dispatch [::events/fetch-demos])
                  (main*)))))
