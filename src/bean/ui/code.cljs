(ns bean.ui.code
  (:require [bean.code :as code]
            [bean.ui.util :refer [cs]]
            [bean.code-errors :as code-errors]))

(defn set-eval-state [sheet]
  (assoc-in sheet
            [:ui :code-evaluation-state]
            (if (code-errors/get-error sheet)
              :error
              :evaluated)))

(defn set-eval-state-pending [sheet]
  (assoc-in sheet [:ui :code-evaluation-state] :pending))

(defn text-area [sheet ui-state]
  [:div {:class :code}
   [:div {:class "code-header bean-label"}
    [:button {:class (cs
                      :small-btn
                      (str "code-state-"
                           (name (or (get-in @sheet [:ui :code-evaluation-state])
                                     :evaluated))))
              :on-click (fn [_]
                          (swap! sheet #(-> %
                                            code/reevaluate
                                            set-eval-state)))}
     "▶"]
    [:div {:class :code-error} (:code-error @sheet)]
    [:button {:class [:small-btn :dark-mode-btn]
              :on-click #(.setAttribute js/document.documentElement "data-theme" "dark")}
     "☾"]
    [:button {:class [:small-btn :light-mode-btn]
              :on-click #(.setAttribute js/document.documentElement "data-theme" "light")}
     "☀"]
    [:button {:class [:small-btn :help-btn]
              :on-click #(swap! ui-state (fn [s] (assoc s :help-display "block")))}
     "?"]]
   [:div {:class :code-thick-lines}]
   [:div {:class :code-body}
    [:div {:class :code-margin}]
    [:textarea
      ;; TODO: The textarea and the code should keep expanding as more text is added
     {:class :code-text
      :content-editable ""
      :on-change (fn [e]
                   (swap! sheet #(-> %
                                     (code/set-code (.-value (.-target e)))
                                     set-eval-state-pending)))
      :spell-check false
      :default-value (code/get-code @sheet)}]]])
