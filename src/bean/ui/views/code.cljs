(ns bean.ui.views.code
  (:require [bean.code :as code]
            [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [cs]]))

(defn text-area []
  (let [sheet (rf/subscribe [::subs/sheet])]
    [:div {:class :code}
     [:div {:class "code-header bean-label"}
      [:button {:class (cs
                        :small-btn
                        (str "code-state-"
                             (name (or (:code-evaluation-state @sheet)
                                       :evaluated))))
                :on-click #(rf/dispatch [::events/evaluate-code])}
       "▶"]
      [:div {:class :code-error} (:code-error @sheet)]
      [:button {:class [:small-btn :dark-mode-btn]
                :on-click #(.setAttribute js/document.documentElement "data-theme" "dark")}
       "☾"]
      [:button {:class [:small-btn :light-mode-btn]
                :on-click #(.setAttribute js/document.documentElement "data-theme" "light")}
       "☀"]
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