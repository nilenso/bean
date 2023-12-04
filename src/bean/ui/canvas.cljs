(ns bean.ui.canvas
  (:require [reagent.core :as rc]))

;; on mousedown canvas, record click start position
;; on mouseup canvas, record click end position
;; find cells that were clicked on
;; if cell is same,
  ;; pointer none off on canvas
  ;; edit mode on that cell
;; else
  ;; draw rectangle
;; when view mode is called, pointer events on canvas

(defn on-mouse-down-handler [])

(defn draw-rect [ctx x y h w]
  (set! (.-lineWidth ctx) 1)
  (.beginPath ctx)
  (.rect ctx x y h w)
  (.stroke ctx))

;; update world
;; paint
;; calculate and 
;; update world

(defn repaint [{:keys [row-heights col-widths]} {:keys [selections]}]
  (let [canvas (.getElementById js/document "bean-canvas")
        ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 1000 1000)
    (doall (for [{:keys [start end]} selections]
             (let [[start-r start-c] start]
               (draw-rect
                ctx
                (apply + (take start-c col-widths))
                (apply + (take start-r row-heights))
                (get col-widths start-c)
                (get row-heights start-r)))))))

(defn canvas [presentation ui-state state-fns]
  (rc/create-class
   {:display-name "bean-canvas"
    :component-did-update
    (fn [this]
      (let [[_ presentation ui-state] (rc/argv this)]
        (repaint presentation ui-state)))

    :reagent-render
    (fn []
      [:canvas {:id :bean-canvas :height 1000 :width 1000}])}))
