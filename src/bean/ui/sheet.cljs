(ns bean.ui.sheet
  (:require [bean.ui.drawing :as drawing]
            [re-frame.core :as rf]
            [bean.ui.subs :as subs]
            [bean.ui.dispatch :as dispatch]
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
         :on-focus (fn [_] (dispatch/edit-mode [row col])) ; Relies on edit mode getting reset on grid evaluation
         :on-key-down #(when (= (.-keyCode %) 13)
                         (.preventDefault %)
                         (-> % .-target .blur)
                         (let [below [(inc row) col]]
                           (.focus (cell-dom-el below))
                           (dispatch/edit-mode below)))
         :on-blur (fn [e]
                    (dispatch/set-mode [row col] :view)
                    (let [input (.-textContent (.-target e))]
                      (set! (.-innerHTML (.-target e)) (:scalar cell))
                      (dispatch/update-cell [row col] input)))
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
             :on-mouse-down #(drawing/resize-top %1 dispatch/resize-col)}
       (util/i->a i)])
    (first rows))])

(defn- row [i cells]
  [:<>
   [:div {:key (str "label" i)
          :data-row i
          :class (cs :bean-label :bean-label-left)
          :on-mouse-down #(drawing/resize-left %1 dispatch/resize-row)}
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
  (let [{:keys [grid ui]} (rf/subscribe [::subs/sheet])]
    [:div
     {:class :bean-sheet
      :id :bean-sheet
      :style {:grid-template-columns (str
                                      "var(--label-left-width) "
                                      (sizes->pxs (:col-widths ui)))
              :grid-template-rows (str
                                   "var(--cell-height) "
                                   (sizes->pxs (:row-heights ui)))}}
     [labels-top grid]
     (map-indexed #(do ^{:key %1}
                    [row %1 %2]) grid)
     [:div {:id :bean-resize-indicator-v}]
     [:div {:id :bean-resize-indicator-h}]
     (when (:selected-cell ui) [cell-selector ui])]))
