(ns bean.ui.views.popups
  (:require [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [re-frame.core :as rf]))

(defn add-labels [frame-name labels]
  (let [dirn-labels (reduce (fn [m [k v]]
                              (update m v (fnil conj #{}) k))
                            {}
                            labels)]
    (doall
     (map (fn [[dirn labels]]
            (rf/dispatch [::events/add-labels frame-name labels dirn]))
          dirn-labels))
    (rf/dispatch [::events/dismiss-popup :add-labels])))

(defn add-labels-popup [suggestions]
  (let [sheet @(rf/subscribe [::subs/sheet])
        grid (:grid sheet)
        {:keys [frame-name labels skip-cells explanation]} suggestions
        existing-labels (get-in sheet [:frames frame-name :labels])]
    [:div {:class "popup"
           :key (random-uuid)}
     [:h4 (str "Add labels in $" frame-name)]
     (for [[label dirn] labels]
       [:div
        {:key label}
        [:input {:type :checkbox
                 :id (str label "-label-checkbox")
                 :defaultChecked (get existing-labels label)
                 :style {:margin 0
                         :margin-right "5px"}
                 :on-change #(if (.-checked (.-target %))
                               (rf/dispatch [::events/add-labels frame-name #{label} dirn])
                               (rf/dispatch [::events/remove-labels frame-name #{label} dirn]))}]
        [:img {:src (str "img/" (name dirn) "-label.png")
               :style {:width "20px"
                       :margin-right "3px"
                       :margin-bottom "3px"
                       :vertical-align "middle"}}]
        [:label {:style {:margin-right "3px"}
                 :for (str label "-label-checkbox")
                 :class :label-suggestion
                 :on-mouse-over #(rf/dispatch [::events/add-preview-labels frame-name #{label} dirn])
                 :on-mouse-out #(rf/dispatch [::events/remove-preview-labels frame-name #{label} dirn])}
         (:content (get-in grid label))]])

     (when-not (empty? skip-cells)
       [:span
        [:p "Skip cells"]
        (for [skip-cell skip-cells]
          [:div
           {:on-mouse-over #(rf/dispatch [::events/mark-skip-cells frame-name #{skip-cell}])
            :on-mouse-out #(when-not (.-checked (.getElementById js/document (str skip-cell "-skip-cell-checkbox")))
                             (rf/dispatch [::events/unmark-skip-cells frame-name #{skip-cell}]))
            :key skip-cell}
           [:input {:type :checkbox
                    :id (str skip-cell "-skip-cell-checkbox")
                    :style {:margin 0
                            :margin-right "5px"}
                    :on-change #(if (.-checked (.-target %))
                                  (rf/dispatch [::events/mark-skip-cells frame-name #{skip-cell}])
                                  (rf/dispatch [::events/unmark-skip-cells frame-name #{skip-cell}]))}]
           [:img {:src (str "img/skip-label.png")
                  :style {:width "20px"
                          :margin-right "3px"
                          :margin-bottom "3px"
                          :vertical-align "middle"}}]
           [:span {:style {:margin-right "3px"}}
            (:content (get-in grid skip-cell))]])])
     [:br]
     [:details
      [:summary [:span {:style {:color "var(--btn-foreground)"}} "Troubleshoot"]]
      [:p explanation]]
     [:br]
     [:button {:class [:controls-btn]
               :on-click #(do
                            (add-labels frame-name labels)
                            (rf/dispatch [::events/mark-skip-cells frame-name skip-cells]))}
      "Accept All"]
     [:button {:class [:controls-btn]
               :on-click #(rf/dispatch [::events/dismiss-popup :add-labels])}
      "Dismiss"]]))

(defn popups []
  [:div {:class :popups}
   (when @(rf/subscribe [::subs/asking-llm])
     [:div {:class [:popup :loader-popup]}
      [:div {:class :llm-loader} "𖣯"]])
   (doall
    (for [[popup-type popup-data] @(rf/subscribe [::subs/popups])]
      (case popup-type
        :add-labels (add-labels-popup popup-data)
        [])))])
