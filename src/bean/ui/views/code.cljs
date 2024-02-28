(ns bean.ui.views.code
  (:require [bean.code :as code]
            [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [cs]]))

(defn text-area []
  (let [sheet (rf/subscribe [::subs/sheet])]
    [:div {:class :code}
     [:div {:class "code-header"}
      [:img {:src "img/code-icon.png"
             :class :code-icon}]
      [:p {:style {:line-height "1.2rem"}}
       "Code"]
      [:div {:class :code-error} (:code-error @sheet)]
      [:button {:class (cs
                        :small-btn
                        :dark-mode-btn
                        (str "code-state-"
                             (name (or (:code-evaluation-state @sheet)
                                       :evaluated))))
                :on-click #(rf/dispatch [::events/evaluate-code])}
       "▶"]
      [:button {:class [:small-btn :help-btn]
                :on-click #(rf/dispatch [::events/display-help true])}
       "?"]]
     [:div {:class :code-thick-lines}]
     [:div {:class :code-body}
      [:div {:class :code-margin}]
      [:textarea
      ;; TODO: The textarea and the code should keep expanding as more text is added
       {:class :code-text
        :content-editable ""
        :on-change #(rf/dispatch [::events/update-code (.-value (.-target %))])
        :spell-check false
        :default-value (code/get-code @sheet)}]]]))
