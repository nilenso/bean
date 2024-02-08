(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.features :as features]
            [re-frame.core :as rf]
            [bean.grid :as grid]))

(defn sidebar []
  (let [table-name (atom nil)]
    (fn []
      [:div
       (when features/show-control-bar
         (let [tables @(rf/subscribe [::subs/tables])
               selection @(rf/subscribe [::subs/selection])
               making-table (:making-table @(rf/subscribe [::subs/ui]))]
           [:div
            (doall
             (for [[table-name] tables]
               [:button {:key table-name
                         :class :controls-btn
                         :on-click #(rf/dispatch [::events/select-table table-name])}
                table-name]))
            (when making-table
              [:form
               {:on-submit #(.preventDefault %)}
               [:input {:auto-focus true
                        :on-change #(reset! table-name (-> % .-target .-value))
                        :placeholder "Table name"}]
               [:button {:class :controls-btn
                         :type :submit
                         :on-click #(rf/dispatch [::events/make-table @table-name selection])}
                "Create Table"]])
            (when-not (grid/area-empty? selection)
              [:button {:class :controls-btn
                        :on-click #(rf/dispatch [::events/making-table])}
               "Make Table"])]))
       [code/text-area]])))
