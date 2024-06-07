(ns bean.ui.views.sidebar
  (:require [bean.ui.views.code :as code]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [re-frame.core :as rf]
            [bean.area :as area]))

(defn frames-list []
  (fn []
    (let [frames @(rf/subscribe [::subs/frames])
          selection @(rf/subscribe [::subs/selection])]
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

             [:a {:on-click #(rf/dispatch [::events/select-frame frame-name])}
              [:img {:src "img/made-frame-icon.png"
                     :class :frame-icon}]
              frame-name]]))]]])))

(defn sidebar []
  [:div {:class :sidebar}
   [:div {:class :logo-container}
    [:img {:src "img/logo.png" :class :bean-logo}]]
   [frames-list]
   [code/text-area]])
