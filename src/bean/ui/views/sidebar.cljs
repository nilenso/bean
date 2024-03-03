(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [re-frame.core :as rf]
            [bean.area :as area]))

(defn tables-list []
  (fn []
    (let [tables @(rf/subscribe [::subs/tables])
          selection @(rf/subscribe [::subs/selection])]
      [:div
       [:div {:class :tables-header}
        [:img {:src "img/table-icon.png"
               :class :table-icon}]
        [:p {:style {:line-height "1.2rem"}}
         "Frames"]
        [:button {:class :controls-btn
                  :style {:margin-left :auto
                          :margin-right "3px"}
                  :disabled (area/area-empty? selection)
                  :on-click #(rf/dispatch [::events/make-frame selection])}
         "Make frame"]]
       [:div {:class :tables-list-items}
        [:div
         (doall
          (for [[table-name] tables]
            [:div {:key table-name
                   :class :tables-list-item}

             [:a {:on-click #(rf/dispatch [::events/select-table table-name])}
              [:img {:src "img/made-table-icon.png"
                     :class :table-icon}]
              table-name]]))]]])))

(defn sidebar []
  [:div {:class :sidebar}
   [:div {:class :logo-container}
    [:img {:src "img/logo.png" :class :bean-logo}]]
   [tables-list]
   [code/text-area]])
