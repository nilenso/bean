(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [re-frame.core :as rf]
            [bean.area :as area]))

(defn frames-list []
  (fn []
    (let [frames @(rf/subscribe [::subs/frames])
          selection @(rf/subscribe [::subs/selection])
          renaming-frame @(rf/subscribe [::subs/renaming-frame])]
      [:div
       [:div {:class :frames-header}
        [:img {:src "img/frame-icon.png"
               :class :frame-icon}]
        [:p {:style {:line-height "1.2rem"}}
         "Frames"]
        [:button {:class :controls-btn
                  :style {:margin-left :auto
                          :margin-right "3px"}
                  :disabled (area/area-empty? selection)
                  :on-click #(rf/dispatch [::events/make-frame selection])}
         "Make frame"]]
       [:div {:class :frames-list-items}
        [:div
         (doall
          (for [[frame-name] frames]
            [:div {:key frame-name
                   :class :frames-list-item}
             [:img {:src "img/made-frame-icon.png"
                    :class :frame-icon}]
             (when (not= renaming-frame frame-name)
               [:span {:style {:display :inherit}}
                [:a {:on-click #(rf/dispatch [::events/renaming-frame frame-name])}
                 frame-name]
                [:a [:img {:src "img/trash-label.png"
                           :on-click #(rf/dispatch [::events/remove-frame frame-name])
                           :style {:margin-left "10px"
                                   :height "0.8rem"}}]]])
             [:div {:class :tables-list-item}
              [:form
               {:on-submit #(do (.preventDefault %)
                                (rf/dispatch [::events/rename-frame
                                              frame-name
                                              (.-value (js/document.getElementById "frame-name-input"))]))
                :class [:make-frame-form]}
               (when (= renaming-frame frame-name)
                 [:input {:class :frame-name-input
                          :id :frame-name-input
                          :auto-focus true
                          :on-focus #(.select (.-target %))
                          :on-blur #(rf/dispatch [::events/renaming-frame nil])
                          :default-value frame-name
                          :placeholder "Frame name"}])]]]))]]])))



(defn sidebar []
  [:div {:class :sidebar}
   [:div {:class :logo-container}
    [:img {:src "img/logo.png" :class :bean-logo}]]
   [frames-list]
   [code/text-area]])
