(ns bean.ui.code
  (:require [bean.code :as code]
            [re-frame.core :as rf]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [cs]]
            [bean.ui.dispatch :as dispatch]))

(defn text-area []
  (let [sheet (rf/subscribe [::subs/sheet])]
    [:div {:class :code}
     [:div {:class "code-header bean-label"}
      [:button {:class (cs
                        :small-btn
                        (str "code-state-"
                             (name (or (get-in sheet [:ui :code-evaluation-state])
                                       :evaluated))))
                :on-click #(dispatch/evaluate-code)}
       "▶"]
      [:div {:class :code-error} (:code-error sheet)]
      [:button {:class [:small-btn :dark-mode-btn]
                :on-click #(.setAttribute js/document.documentElement "data-theme" "dark")}
       "☾"]
      [:button {:class [:small-btn :light-mode-btn]
                :on-click #(.setAttribute js/document.documentElement "data-theme" "light")}
       "☀"]
      [:button {:class [:small-btn :help-btn]
                :on-click #(dispatch/display-help true)}
       "?"]]
     [:div {:class :code-thick-lines}]
     [:div {:class :code-body}
      [:div {:class :code-margin}]
      [:textarea
      ;; TODO: The textarea and the code should keep expanding as more text is added
       {:class :code-text
        :content-editable ""
        :on-change #(dispatch/update-code (.-value (.-target %)))
        :spell-check false
        :default-value (code/get-code sheet)}]]]))
