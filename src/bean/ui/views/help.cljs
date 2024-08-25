(ns bean.ui.views.help
  (:require [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]))

(defn help []
  (let [ui (rf/subscribe [::subs/ui])
        anthropic-api-key @(rf/subscribe [::subs/anthropic-api-key])]
    [:div {:id :help-overlay
           :class :help-overlay
           :style {:display (if (:help-display @ui) "block" "none")}
           :on-click #(rf/dispatch [::events/display-help false])}
     [:div {:class :help-container
            :on-click #(.stopPropagation %)}
      [:div
       {:class :help-content}
       [:img {:src "help.png" :class :help-light :width "100%"}]
       [:img {:src "help-dark.png" :class :help-dark :width "100%"}]
       [:p {:class :footer-p} "𖣯 Anthropic API Key"]
       [:input {:type "password"
                :value anthropic-api-key
                 :on-change #(rf/dispatch [::events/set-anthropic-api-key (-> % .-target .-value)])}]]
      [:div
       {:class :help-footer}
       [:p {:class :footer-p}
        "How to use Bean"]
       [:a {:href "https://github.com/nilenso/bean"
            :class :footer-github-link} "What is this?"]]]]))
