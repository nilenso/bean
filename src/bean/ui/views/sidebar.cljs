(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.features :as features]
            [re-frame.core :as rf]))

(defn sidebar []
  [:div
   (when features/show-control-bar
     (let [tables @(rf/subscribe [::subs/tables])
           {:keys [start end]} @(rf/subscribe [::subs/selection])]

       [:div
        (doall
         (for [[table-name table-data] tables]
           [:button {:key table-name
                     :class :controls-btn
                     :on-click #(rf/dispatch [::events/select-table table-data])}
            table-name]))
        [:button {:class :controls-btn
                  :on-click #(rf/dispatch [::events/make-table (str "Table" (rand-int 100)) start end])}
         "Make Table"]]))
   [code/text-area]])
