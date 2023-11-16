(ns bean.ui.scratch
  (:require [bean.scratch :as scratch]
            [bean.ui.util :refer [cs]]))

(defn set-eval-state [sheet]
  (assoc-in sheet
            [:ui :scratch-evaluation-state]
            (if (:errors sheet)
              :error
              :evaluated)))

(defn set-eval-state-pending [sheet]
  (assoc-in sheet [:ui :scratch-evaluation-state] :pending))

(defn text-area [sheet ui-state]
  [:div {:class :scratch}
   [:div {:class "scratch-header bean-label"}
    [:button {:class (cs
                      :small-btn
                      (str "scratch-state-"
                           (name (or (get-in @sheet [:ui :scratch-evaluation-state])
                                     :evaluated))))
              :on-click (fn [_]
                          (swap! sheet #(-> %
                                            scratch/reevaluate
                                            set-eval-state)))}
     "▶"]
    [:div {:class :scratch-error} (:code-error @sheet)]
    [:button {:class [:small-btn :help-btn]
              :on-click #(swap! ui-state (fn [s] (assoc s :help-display "block")))}
     "?"]
    #_[:button {:class [:small-btn :dark-mode-btn]
                :on-click #(.setAttribute js/document.documentElement "data-theme" "dark")}
       "☾"]
    #_[:button {:class [:small-btn :light-mode-btn]
                :on-click #(.setAttribute js/document.documentElement "data-theme" "light")}
       "☀"]]
   [:div {:class :scratch-thick-lines}]
   [:div {:class :scratch-body}
    [:div {:class :scratch-margin}]
    [:textarea
      ;; TODO: The textarea and the scratch should keep expanding as more text is added
     {:class :scratch-text
      :content-editable ""
      :on-change (fn [e]
                   (swap! sheet #(-> %
                                     (scratch/set-code (.-value (.-target e)))
                                     set-eval-state-pending)))
      :spell-check false
      :default-value (scratch/get-code @sheet)}]]])
