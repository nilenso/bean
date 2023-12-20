(ns bean.ui.views.code
  (:require [bean.code :as code]
            [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [cs rc->a1 a1->rc]]
            [clojure.string :as str]))

(defn- rect-bounds [corner opposite-corner]
  (let [[r1 c1] corner
        [r2 c2] opposite-corner]
    (str/join ":"
              [(rc->a1 (min r1 r2) (min c1 c2))
               (rc->a1 (max r1 r2) (max c1 c2))])))

(defn- selections->range [selections]
  ;; TODO: Unsure why multiple selections are possible
  (first
   (map #(rect-bounds (:start %) (:end %)) selections)))

(def ^:private selection-regex #"^([A-Z]+)([0-9]+):([A-Z]+)([0-9]+)$")

(defn- valid-selection [selection-text]
  (when (string? selection-text)
    (when-let [[_ a1 n1 a2 n2] (re-find selection-regex selection-text)]
      [(a1->rc a1 n1) (a1->rc a2 n2)])))

(defn text-area []
  (let [sheet (rf/subscribe [::subs/sheet])
        ui-selections (rf/subscribe [::subs/ui-selections])
        selection (-> @ui-selections
                      selections->range
                      valid-selection)]
    [:div {:class :code}
     [:div {:class "code-header bean-label"}
      [:button {:class (cs
                        :small-btn
                        (str "code-state-"
                             (name (or (get-in @sheet [:ui :code-evaluation-state])
                                       :evaluated))))
                :on-click #(rf/dispatch [::events/evaluate-code])}
       "▶"]
      [:div {:class :code-error} (:code-error @sheet)]
      [:input {:type :text
               :default-value (selections->range @ui-selections)
               :class (cs :table-address-input)}]
      [:button (cond-> {:class (cs
                                :small-btn
                                :table-create-crosstab)
                        :on-click (fn [_]
                                    (prn "Create HTab" (str selection)))}
                 (nil? selection) (assoc :disabled true))
       "▤"]
      [:button (cond-> {:class (cs
                                :small-btn
                                :table-create-crosstab)
                        :on-click (fn [_]
                                    (prn "Create VTab" (str selection)))}
                 (nil? selection) (assoc :disabled true))
       "▥"]
      [:button (cond-> {:class (cs
                                :small-btn
                                :table-create-crosstab)
                        :on-click (fn [_]
                                    (prn "Create XTab" (str selection)))}
                 (nil? selection) (assoc :disabled true))
       "▦"]
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