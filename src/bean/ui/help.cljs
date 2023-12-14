(ns bean.ui.help
  (:require [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]))

(defn help []
  (let [ui (rf/subscribe [::subs/ui])]
    [:div {:id :help-overlay
           :class :help-overlay
           :style {:display (if (:help-display @ui) "block" "none")}
           :on-click #(rf/dispatch [::events/display-help false])}
     [:div {:class :help-container}
      [:div
       {:class :help-content}
       [:img {:src "help.png" :class :help-light :width "100%"}]
       [:img {:src "help-dark.png" :class :help-dark :width "100%"}]]
      [:div
       {:class :help-footer}
       [:p {:class :footer-p}
        "How to use Bean"]
       [:a {:href "https://github.com/nilenso/bean"
            :class :footer-github-link} "What is this?"]]]]))
