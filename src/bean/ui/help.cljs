(ns bean.ui.help
  (:require [re-frame.core :as re-frame]
            [bean.ui.subs :as subs]
            [bean.ui.dispatch :as dispatch]))

(defn help []
  (let [ui (re-frame/subscribe [::subs/ui])]
    [:div {:id :help-overlay
           :class :help-overlay
           :style {:display (if (:help-display ui) "block" "none")}
           :on-click #(dispatch/display-help false)}
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
