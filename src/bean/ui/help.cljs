(ns bean.ui.help)

(defn help [ui-state]
  [:div {:id :help-overlay
         :class :help-overlay
         :style {:display (:help-display @ui-state)}
         :on-click #(swap! ui-state (fn [s] (assoc s :help-display "none")))}
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
          :class :footer-github-link} "What is this?"]]]])
