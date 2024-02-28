(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [re-frame.core :as rf]
            [bean.area :as area]))

(defn tables-list []
  (let [table-name (atom nil)]
    (fn []
      (let [tables @(rf/subscribe [::subs/tables])
            selection @(rf/subscribe [::subs/selection])
            making-table (:making-table @(rf/subscribe [::subs/ui]))]
        [:div
         [:div {:class :tables-header}
          [:img {:src "img/table-icon.png"
                 :class :table-icon}]
          [:p {:style {:line-height "1.2rem"}}
           "Tables"]
          [:button {:class :controls-btn
                    :style {:margin-left :auto
                            :margin-right "3px"}
                    :disabled (area/area-empty? selection)
                    :on-click #(rf/dispatch [::events/making-table])}
           "Make Table"]]
         [:div {:class :tables-list-items}
          [:div
           (doall
            (for [[table-name] tables]
              [:div {:key table-name
                     :class :tables-list-item}

               [:a {:on-click #(rf/dispatch [::events/select-table table-name])}
                [:img {:src "img/made-table-icon.png"
                       :class :table-icon}]
                table-name]]))
           (when (and making-table (not (area/area-empty? selection)))
             [:div {:class :tables-list-item}
              [:form
               {:on-submit #(.preventDefault %)
                :class [:make-table-form]}
               [:input {:class :make-table-input
                        :auto-focus true
                        :on-change #(reset! table-name (-> % .-target .-value))
                        :placeholder "Table name"}]
               [:button {:class :controls-btn
                         :style {:margin-left "-3px"
                                 :margin-right "0"
                                 :height "26px"}
                         :type :submit
                         :on-click #(rf/dispatch [::events/make-table @table-name selection])}
                "Create Table"]]])]]]))))

(defn sidebar []
  [:div {:class :sidebar}
   [:div {:class :logo-container}
    [:img {:src "img/logo.png" :class :bean-logo}]]
   [tables-list]
   [code/text-area]])
