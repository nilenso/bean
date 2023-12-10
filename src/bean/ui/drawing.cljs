(ns bean.ui.drawing
  (:require [bean.ui.util :refer [px]]
            [reagent.core :as rc]))

;; on mousedown canvas, record click start position
;; on mouseup canvas, record click end position
;; find cells that were clicked on
;; if cell is same,
  ;; pointer none off on canvas
  ;; edit mode on that cell
;; else
  ;; draw rectangle
;; when view mode is called, pointer events on canvas

(defonce canvas-state (rc/atom {}))

(defn px->index [px offsets]
  (if (> px (reduce + offsets)) 
    -1
    (reduce
     (fn [{:keys [index position]} next-height]
       (if (<= (+ position next-height) px)
         {:position (+ position next-height)
          :index (inc index)}
         (reduced index)))
     {:index 0 :position 0}
     offsets)))

(defn xy->rc [[x y] row-heights col-widths]
  [(px->index y row-heights) (px->index x col-widths)])

(defn draw-rect [ctx x y h w]
  (set! (.-lineWidth ctx) 0.1)
  (.beginPath ctx)
  (.rect ctx x y h w)
  (.stroke ctx))

(defn selection->rect [ctx start row-heights col-widths]
  (let [[start-r start-c] start]
    (draw-rect
     ctx
     (- (apply + (take start-c col-widths)) 1)
     (- (apply + (take start-r row-heights)) 1)
     (+ (get col-widths start-c) 3)
     (+ (get row-heights start-r) 3))))

(defn on-mouse-down [e {:keys [row-heights col-widths]} edit-mode-fn]
  (let [x (.-offsetX (.-nativeEvent e))
        y (.-offsetY (.-nativeEvent e))] 
    (.stopPropagation e)
    (reset! canvas-state {})
    (swap! canvas-state assoc :click-start [x y])
    (edit-mode-fn (xy->rc [x y] row-heights col-widths))))

(defn on-mouse-up [e]
  (let [x (.-offsetX (.-nativeEvent e))
        y (.-offsetY (.-nativeEvent e))]
    (swap! canvas-state assoc :click-end [x y])))

(defn repaint [{:keys [row-heights col-widths]} {:keys [selections]}]
  (let [canvas (.getElementById js/document "bean-canvas")
        ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 1000 1000)
    #_(doall (for [{:keys [start end]} selections]
             (selection->rect ctx start row-heights col-widths)))))

(defn canvas [presentation ui-state state-fns]
  (rc/create-class
   {:display-name "bean-canvas"
    :component-did-update
    (fn [this [_ old-presentation old-ui-state]]
      (let [[_ presentation ui-state] (rc/argv this)]
        (repaint presentation ui-state)))

    :reagent-render
    (fn [presentation ui-state state-fns]
      [:canvas {:id :bean-canvas
                :height 1000
                :width 1000
                :on-mouse-down #(on-mouse-down %1 presentation (:edit-mode state-fns))
                :on-mouse-up on-mouse-up}])}))

(defn resize-top [e resize-fn]
  (let [sheet (.getElementById js/document "bean-sheet")
        indicator (.getElementById js/document "bean-resize-indicator-v")
        label (.-target e)
        label-begin (.-offsetLeft label)
        col (js/parseInt (.getAttribute label "data-col"))
        sheet-begin (.-left (.getBoundingClientRect sheet))
        sheet-click-pos #(- (.-clientX %) sheet-begin)]
    (letfn [(move-indicator
              [e2]
              (set! (.-left (.-style indicator)) (px (sheet-click-pos e2))))
            (update-size
              [e2]
              (let [new-size (- (sheet-click-pos e2) label-begin)]
                (when (pos? new-size)
                  (resize-fn col new-size))
                (.removeEventListener js/document "mousemove" move-indicator)
                (.removeEventListener js/document "mouseup" update-size)
                (set! (.. indicator -style -display) "none")
                (set! (.. sheet -style -cursor) "auto")))]
      (when
       (< (- (+ label-begin (.-offsetWidth label)) (sheet-click-pos e)) 10)
        (set! (.. indicator -style -display) "block")
        (set! (.. sheet -style -cursor) "ew-resize")
        (move-indicator e)
        (.addEventListener js/document "mousemove" move-indicator)
        (.addEventListener js/document "mouseup" update-size)))))

(defn resize-left [e resize-fn]
  (let [sheet (.getElementById js/document "bean-sheet")
        indicator (.getElementById js/document "bean-resize-indicator-h")
        label (.-target e)
        label-begin (.-offsetTop label)
        row (js/parseInt (.getAttribute label "data-row"))
        sheet-begin (.-top (.getBoundingClientRect sheet))
        sheet-click-pos #(- (.-clientY %) sheet-begin)]
    (letfn [(move-indicator
              [e2]
              (set! (.-top (.-style indicator)) (px (sheet-click-pos e2))))
            (update-size
              [e2]
              (let [new-size (- (sheet-click-pos e2) label-begin)]
                (when (pos? new-size)
                  (resize-fn row new-size))
                (.removeEventListener js/document "mousemove" move-indicator)
                (.removeEventListener js/document "mouseup" update-size)
                (set! (.. indicator -style -display) "none")
                (set! (.. sheet -style -cursor) "auto")))]
      (when (< (- (+ label-begin (.-offsetHeight label)) (sheet-click-pos e)) 8)
        (set! (.. indicator -style -display) "block")
        (set! (.. sheet -style -cursor) "ns-resize")
        (move-indicator e)
        (.addEventListener js/document "mousemove" move-indicator)
        (.addEventListener js/document "mouseup" update-size)))))
