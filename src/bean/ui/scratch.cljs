(ns bean.ui.scratch
  (:require [bean.scratch :as scratch]
            [bean.ui.util :refer [cs] :as util]
            [bean.ui.sheet :as sheet]))

(defn set-eval-state [sheet]
(prn "UIII" (get sheet :ui))
  (assoc-in sheet
            [:ui :scratch-evaluation-state]
            (if (:errors sheet)
              :error
              :evaluated)))

(defn set-eval-state-pending [sheet]
  (prn "2I" (get sheet :ui))
  (assoc-in sheet [:ui :scratch-evaluation-state] :pending))

(defn text-area [sheet]
  (prn (get @sheet :ui))
  [:div {:class (cs (str "scratch-state-"
                      (name (or (get-in @sheet [:ui :scratch-evaluation-state])
                                :evaluated))) 
                 :scratch)}
   [:div {:class "scratch-header bean-label"}
    [:button {:on-click (fn [e]
                          (swap! sheet #(-> %
                                            scratch/reevaluate
                                            set-eval-state))
                          (prn "SHETETET" @sheet))}
     "▶"]]
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
