(ns bean.ui.views.drawing
  (:require [bean.ui.util :refer [px]]
            [re-frame.core :as rf]
            [bean.ui.subs :as subs]
            [bean.ui.events :as events]
            [reagent.core :as rc]))

(def selection-fill "rgba(0, 0, 0, 0.03)")
(def selection-border "rgb(150, 150, 150)")
(def canvas-height 600)
(def canvas-width 1500)

(defn resize-top [e]
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
                  (rf/dispatch [::events/resize-col col new-size]))
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

(defn resize-left [e]
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
                  (rf/dispatch [::events/resize-row row new-size]))
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

(defn px->index [px offsets]
  (if (> px (reduce + offsets))
    -1
    (reduce
     (fn [{:keys [index current-pixel]} next-height]
       (if (<= (+ current-pixel next-height) px)
         {:index (inc index)
          :current-pixel (+ current-pixel next-height)}
         (reduced index)))
     {:index 0 :current-pixel 0}
     offsets)))

(defn xy->rc [[x y] row-heights col-widths]
  [(px->index y row-heights) (px->index x col-widths)])

(defn draw-rect [ctx x y h w fill border]
  (set! (.-lineWidth ctx) 1)
  (set! (.-fillStyle ctx) fill)
  (.beginPath ctx)
  (.fillRect ctx x y h w)
  (set! (.-strokeStyle ctx) border)
  (.rect ctx x y h w)
  (.stroke ctx))

(defn selection->rect [ctx start end row-heights col-widths]
  (let [[start-r start-c] start
        [end-r end-c] end
        [top-r top-c] [(min start-r end-r) (min start-c end-c)]
        [bottom-r bottom-c] [(max start-r end-r) (max start-c end-c)]]
    (draw-rect
     ctx
     (apply + (take top-c col-widths))
     (apply + (take top-r row-heights))
     (reduce + (subvec col-widths top-c (inc bottom-c)))
     (reduce + (subvec row-heights top-r (inc bottom-r)))
     selection-fill
     selection-border)))

(defn on-mouse-down [e {:keys [row-heights col-widths]}]
  (let [x (.-offsetX (.-nativeEvent e))
        y (.-offsetY (.-nativeEvent e))
        [r c] (xy->rc [x y] row-heights col-widths)]
    (.preventDefault e)
    (rf/dispatch [::events/clear-selections])
    (rf/dispatch [::events/start-selection [r c]])
    (rf/dispatch [::events/edit-mode [r c]])))

(defn on-mouse-move [e {:keys [selection-start]} {:keys [row-heights col-widths]}]
  (when selection-start
    (let [x (.-offsetX (.-nativeEvent e))
          y (.-offsetY (.-nativeEvent e))
          start-rc selection-start
          end-rc (xy->rc [x y] row-heights col-widths)]
      (.preventDefault e)
      (when (not= start-rc end-rc)
        (rf/dispatch-sync [::events/clear-selections])
        (rf/dispatch-sync [::events/make-selection {:start start-rc :end end-rc}])))))

(defn on-mouse-up []
  (rf/dispatch [::events/finish-selection]))

(defn paint [{:keys [row-heights col-widths]} {:keys [selections]}]
  (let [canvas (.getElementById js/document "bean-canvas")
        ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 canvas-width canvas-height)
    (doall (for [{:keys [start end]} selections]
             (selection->rect ctx start end row-heights col-widths)))))

(defn- canvas* []
  (rc/create-class
   {:display-name "bean-canvas"
    :component-did-update
    (fn [this _]
      (let [[_ sheet ui] (rc/argv this)]
        (paint (:grid-dimensions sheet) ui)))

    :reagent-render
    (fn [sheet ui]
      [:canvas {:id :bean-canvas
                :height 600
                :width 1500
                :on-mouse-down #(on-mouse-down %1 (:grid-dimensions sheet))
                :on-mouse-move #(on-mouse-move %1 ui (:grid-dimensions sheet))
                :on-mouse-up on-mouse-up}])}))

(defn canvas []
  (let [sheet (rf/subscribe [::subs/sheet])
        ui (rf/subscribe [::subs/ui])]
    [canvas* @sheet @ui]))
