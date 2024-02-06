(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.features :as features]
            [re-frame.core :as rf]))

(defn sidebar []
  (let [table-name (atom nil)]
    (fn []
      [:div
       (when features/show-control-bar
         (let [tables @(rf/subscribe [::subs/tables])
               {:keys [start end]} @(rf/subscribe [::subs/selection])
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
                         :on-click #(rf/dispatch [::events/make-table @table-name start end])}
                "Create Table"]])
            (when-not (= start end)
              [:button {:class :controls-btn
                        :on-click #(rf/dispatch [::events/making-table])}
               "Make Table"])]))
       [code/text-area]])))
