(ns bean.ui.views.sheet
  (:require [bean.ui.views.drawing :as drawing]
            [re-frame.core :as rf]
            [bean.ui.events :as events]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [px cs] :as util]))

(defn- sizes->pxs [sizes]
  (reduce #(str %1 (px %2) " ") "" sizes))

;; TODO: use the reframe keyboard library here
(defn- handle-cell-navigation [e [row col]]
  (let [move-to-cell (cond
                       (and (= (.-keyCode e) 13) (.-shiftKey e)) [(dec row) col]
                       (and (= (.-keyCode e) 9) (.-shiftKey e)) [row (dec col)]
                       (= (.-keyCode e) 13) [(inc row) col]
                       (= (.-keyCode e) 9) [row (inc col)])]
    ;; TODO: These side-effects need to move to an rf event handler
    (when move-to-cell
      (.preventDefault e)
      (-> e .-target .blur)
      (rf/dispatch [::events/edit-mode move-to-cell]))))

(defn- cell [row col {:keys [mode error content representation] :as cell}]
  [:div {:content-editable true
         :suppressContentEditableWarning true
         :data-row row
         :data-col col
         :on-mouse-up #(rf/dispatch [::events/finish-selection])
         :on-mouse-down #(rf/dispatch [::events/clear-selections])
         :on-key-down #(handle-cell-navigation % [row col])
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

(defn sheet []
  [:div {:id :grid-container}
   [drawing/canvas]
   [drawing/cell-input]])
