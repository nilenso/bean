(ns bean.ui.sheet
  (:require [bean.ui.drawing :as drawing]
            [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [px cs] :as util]))

(defn- cell-dom-el
  [[row col]]
  (.querySelector
   js/document
   (str "[data-row=\"" row "\"][data-col=\"" col "\"]")))

(defn- sizes->pxs [sizes]
  (reduce #(str %1 (px %2) " ") "" sizes))

(defn- cell [row col {:keys [mode error content representation] :as cell}]
  [:div {:content-editable true
         :suppressContentEditableWarning true
         :data-row row
         :data-col col
         :on-focus #(rf/dispatch [::events/set-mode [row col] :edit]) ; Relies on edit mode getting reset on grid evaluation
         :on-key-down #(when (= (.-keyCode %) 13)
                         ;; TODO: These side-effects need to move to an rf event handler
                         (.preventDefault %)
                         (-> % .-target .blur)
                         (let [below [(inc row) col]]
                           (.focus (cell-dom-el below))
                           (rf/dispatch [::events/set-mode below :edit])))
         :on-blur (fn [e]
                    (rf/dispatch [::events/set-mode [row col] :view])
                    ;; TODO: Move more of this into the event handler?
                    (let [target (.-target e)
                          txt (str (.-textContent target))]
                      (set! (.-innerHTML target) (:scalar cell))
                      (rf/dispatch [::events/update-cell [row col] txt])))
         :class (cs :bean-cell
                    (when (= mode :edit) :edit-mode)
                    (when error :cell-error))
         :spell-check :false}
   (case mode
     :edit content
     representation)])

(defn- labels-top [rows]
  [:<>
   [:div {:class (cs :bean-label :bean-corner)}]
   (map-indexed
    (fn [i _] ^{:key i}
      [:div {:key (str "label" i)
             :data-col i
             :class (cs :bean-label :bean-label-top)
             :on-mouse-down #(drawing/resize-top %1)}
       (util/i->a i)])
    (first rows))])

(defn- row [i cells]
  [:<>
   [:div {:key (str "label" i)
          :data-row i
          :class (cs :bean-label :bean-label-left)
          :on-mouse-down #(drawing/resize-left %1)}
    (inc i)]
   (map-indexed #(do ^{:key %1}
                  [cell i %1 %2])
                cells)])

(defn cell-selector [{:keys [selected-cell row-heights col-widths]}]
  (let [[r c] selected-cell]
    [:div
     {:id :cell-selector
      :style {:top (px (+ 30 (apply + (take r row-heights))))
              :left (px (+ 40 (apply + (take c col-widths))))
              :display :block
              :height (get row-heights r)
              :width (get col-widths c)}}]))

(defn sheet []
  (let [sheet (rf/subscribe [::subs/sheet])]
    [:div
     {:class :bean-sheet
      :id :bean-sheet
      :style {:grid-template-columns (str
                                      "var(--label-left-width) "
                                      (sizes->pxs (:col-widths (:ui @sheet))))
              :grid-template-rows (str
                                   "var(--cell-height) "
                                   (sizes->pxs (:row-heights (:ui @sheet))))}}
     [labels-top (:grid @sheet)]
     (map-indexed #(do ^{:key %1}
                    [row %1 %2]) (:grid @sheet))
     [:div {:id :bean-resize-indicator-v}]
     [:div {:id :bean-resize-indicator-h}]
     (when (:selected-cell (:ui @sheet)) [cell-selector (:ui @sheet)])]))
