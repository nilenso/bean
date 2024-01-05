(ns bean.ui.views.drawing
  (:require [bean.ui.util :refer [px]]
            [re-frame.core :as rf]
            [bean.ui.subs :as subs]
            [bean.ui.events :as events]
            [bean.ui.styles :as styles]
            [pixi.js :as pixi]
            [pixi-viewport :as pixi-viewport]
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

(defn paint1 [{:keys [row-heights col-widths]} {:keys [selections]}]
  (let [canvas (.getElementById js/document "bean-canvas")
        ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 canvas-width canvas-height)
    (doall (for [{:keys [start end]} selections]
             (selection->rect ctx start end row-heights col-widths)))))

(defn- draw-line [g width color sx sy ex ey]
  (.lineStyle g width color 1 0.5 true)
  (.moveTo g sx sy)
  (.lineTo g ex ey)
  (.lineStyle g 0 color 1 0.5 true))

(defn- corner [viewport]
  (let [g (new pixi/Graphics)]
    (.beginFill g (:corner-background styles/colors))
    (.drawRect g 0 0 (:heading-left-width styles/sizes) (:cell-height styles/sizes))
    (.on viewport "moved" #(set! (.. g -position -x) (.-left viewport)))
    (.on viewport "moved" #(set! (.. g -position -y) (.-top viewport)))
    g))

(defn- top-heading [row-heights viewport]
  (let [g (new pixi/Graphics)]
    (.beginFill g (:heading-background styles/colors))
    (.drawRect g 0 0 (:heading-left-width styles/sizes) 10000)
    (->> row-heights
         (reductions +)
         (map #(draw-line g 0.5 (:heading-border styles/colors) 0 % (:heading-left-width styles/sizes) %))
         dorun)
    (set! (.. g -position -y) (:cell-height styles/sizes))
    (.on viewport "moved" #(set! (.. g -position -x) (.-left viewport)))
    g))

(defn- left-heading [col-widths viewport]
  (let [g (new pixi/Graphics)]
    (.beginFill g (:heading-background styles/colors))
    (.drawRect g 0 0 10000 (:cell-height styles/sizes))
    (->> col-widths
         (reductions +)
         (map #(draw-line g 0.5 (:heading-border styles/colors) % 0 % (:cell-height styles/sizes)))
         dorun)
    (set! (.. g -position -x) (:heading-left-width styles/sizes))
    (.on viewport "moved" #(set! (.. g -position -y) (.-top viewport)))
    g))

(defn- grid [row-heights col-widths]
  (let [g (new pixi/Graphics)]
    (letfn [(draw-line*
              [sx sy ex ey]
              (draw-line g
                         (:grid-line styles/sizes)
                         (:grid-line styles/colors) sx sy ex ey))
            (draw-horizontal [y] (draw-line* 0 y 10000 y))
            (draw-vertical [x] (draw-line* x 0 x 10000))]
      (set! (.. g -position -x) (:heading-left-width styles/sizes))
      (set! (.. g -position -y) (:cell-height styles/sizes))
      (dorun (->> row-heights (reductions +) (map draw-horizontal)))
      (dorun (->> col-widths (reductions +) (map draw-vertical))))
    g))

(defn paint [{:keys [row-heights col-widths]} {:keys [pixi selections]}]
  (let [v (:viewport pixi)
        c (:container pixi)]
    (.removeChildren c)
    (.addChild c (grid row-heights col-widths))
    (.addChild c (top-heading row-heights v))
    (.addChild c (left-heading col-widths v))
    (.addChild c (corner v))))

(defn- make-app []
  (new
   pixi/Application
   #js {:autoResize true
        :resizeTo (.getElementById js/document "canvas-container")
        :resolution (.-devicePixelRatio js/window),
        :backgroundColor (:sheet-background styles/colors)
        :autoDensity true}))

(defn- make-viewport [app]
  (new
   pixi-viewport/Viewport
   #js {:events (.. app -renderer -events)
        :screenWidth (.-offsetWidth (.getElementById js/document "canvas-container"))
        :screenHeight (.-offsetHeight (.getElementById js/document "canvas-container"))
        :worldWidth 10000
        :worldHeight 10000}))

(defn setup []
  (let [app (make-app)
        viewport (make-viewport app)]
    (rf/dispatch [::events/set-pixi-container app viewport (new pixi/Container)])))

(defn- canvas* []
  (rc/create-class
   {:display-name "bean-canvas"
    :component-did-mount setup

    :component-did-update
    (fn [this _]
      (let [[_ sheet ui] (rc/argv this)]
        (when (:pixi ui)
          (paint (:grid-dimensions sheet) ui))))

    :reagent-render
    (fn [])}))

(defn canvas []
  (let [sheet (rf/subscribe [::subs/sheet])
        ui (rf/subscribe [::subs/ui])]
    [canvas* @sheet @ui]))
