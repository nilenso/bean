(ns bean.ui.drawing
  (:require [bean.ui.util :refer [px]]
            [reagent.core :as rc]))

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
  (set! (.-lineWidth ctx) 1)
  (set! (.-fillStyle ctx) "rgba(0, 0, 0, 0.03)")
  (.beginPath ctx)
  (.fillRect ctx x y h w)
  (set! (.-strokeStyle ctx) "rgb(150, 150, 150)")
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
     (reduce + (subvec row-heights top-r (inc bottom-r))))))

(defn on-mouse-down [e {:keys [row-heights col-widths]} edit-mode-fn update-selections] 
  (let [x (.-offsetX (.-nativeEvent e))
        y (.-offsetY (.-nativeEvent e))
        [r c] (xy->rc [x y] row-heights col-widths)]
    (.preventDefault e)
    (edit-mode-fn [r c])
    (update-selections [])
    (.focus (js/document.getElementById (str "cell-" r "-" c)))
    (swap! canvas-state assoc :click-start [r c])))

(defn on-mouse-move [e {:keys [row-heights col-widths]} update-selections]
  (when (:click-start @canvas-state)
    (let [x (.-offsetX (.-nativeEvent e))
          y (.-offsetY (.-nativeEvent e))
          start-rc (:click-start @canvas-state)
          end-rc (xy->rc [x y] row-heights col-widths)]
      (.preventDefault e)
      (when (not= start-rc end-rc)
        (update-selections [{:start start-rc :end end-rc}])))))

(defn on-mouse-up []
  (swap! canvas-state assoc :click-start nil))

(defn paint [{:keys [row-heights col-widths]} {:keys [selections]}]
  (let [canvas (.getElementById js/document "bean-canvas")
        ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 1500 600)
    (doall (for [{:keys [start end]} selections]
             (selection->rect ctx start end row-heights col-widths)))))

(defn canvas [presentation ui-state state-fns]
  (rc/create-class
   {:display-name "bean-canvas"
    :component-did-update
    (fn [this [_ old-presentation old-ui-state]]
      (let [[_ presentation ui-state] (rc/argv this)]
        (paint presentation ui-state)))

    :reagent-render
    (fn [presentation ui-state state-fns]
      [:canvas {:id :bean-canvas
                :height 600
                :width 1500
                :on-mouse-down #(on-mouse-down %1 presentation (:edit-mode state-fns) (:update-selections state-fns))
                :on-mouse-move #(on-mouse-move %1 presentation (:update-selections state-fns))
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
