(ns bean.ui.views.drawing
  (:require [bean.ui.events :as events]
            [bean.ui.styles :as styles]
            [bean.ui.subs :as subs]
            [bean.ui.util :refer [px] :as util]
            [pixi-viewport :as pixi-viewport]
            [pixi.js :as pixi]
            [re-frame.core :as rf]
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

(defn- center-text! [bitmap-text x y h w]
  (let [text-h (.-height bitmap-text)
        text-w (.-width bitmap-text)]
    (set! (.-x bitmap-text) (- (+ x (/ w 2)) (/ text-w 2)))
    (set! (.-y bitmap-text) (- (+ y (/ h 2)) text-h)))
  bitmap-text)

(defn- heading-text [g text x y h w]
  (let [bitmap (new
                pixi/BitmapText
                text
                #js {:fontName "SpaceGrotesk"
                     :tint (:heading-color styles/colors)
                     :fontSize (:heading-font styles/sizes)})]
    (->> (center-text! bitmap x y h w) (.addChild g))))

(defn- native-line [g color sx sy ex ey]
   ;; native lines can be 1px wide only.
  (.lineStyle g 1 color 1 0.5 true)
  (.moveTo g sx sy)
  (.lineTo g ex ey)
  (.lineStyle g 0 color 1 0.5 true))

(defn- grid-line [g sx sy ex ey]
  (native-line g (:grid-line styles/colors) sx sy ex ey))

(defn- heading-line [g sx sy ex ey]
  (native-line g (:heading-border styles/colors) sx sy ex ey))

(defn- corner [viewport]
  (let [g (new pixi/Graphics)
        reposition #(do (set! (.. g -position -x) (.-left viewport))
                        (set! (.. g -position -y) (.-top viewport)))]
    (.beginFill g (:corner-background styles/colors))
    (.drawRect g 0 0 (:heading-left-width styles/sizes) (:cell-h styles/sizes))
    (reposition)
    (.on viewport "moved" reposition)
    g))

(defn- left-heading [row-heights viewport]
  (let [g (new pixi/Graphics)
        offset-t (:cell-h styles/sizes)
        offset-l (:heading-left-width styles/sizes)
        reposition #(set! (.. g -position -x) (.-left viewport))]
    ;; draw the background
    (.beginFill g (:heading-background styles/colors))
    ;; this should be relative to viewport x and y, not 0 0
    (.drawRect g 0 0 offset-l (:world-h styles/sizes))
    (heading-line g offset-l 0 offset-l (:world-h styles/sizes))
    ;; draw individual label borders
    (->> row-heights
         (reductions + offset-t)
         (map #(heading-line g 0 % offset-l %))
         dorun)
    ;; draw text
    (reduce
     (fn [y [idx w]]
       (heading-text g (inc idx) 0 y w offset-l)
       (+ y w))
     offset-t (map-indexed vector row-heights))
    (reposition)
    (.on viewport "moved" reposition)
    g))

(defn- top-heading [col-widths viewport]
  (let [g (new pixi/Graphics)
        offset-t (:cell-h styles/sizes)
        offset-l (:heading-left-width styles/sizes)
        reposition #(set! (.. g -position -y) (.-top viewport))]
    (.beginFill g (:heading-background styles/colors))
    (.drawRect g 0 0 (:world-w styles/sizes) offset-t)
    (heading-line g 0 offset-t (:world-w styles/sizes) offset-t)
    (->> col-widths
         (reductions + offset-l)
         (map #(heading-line g % 0 % offset-t))
         dorun)
    (reduce
     (fn [x [idx w]]
       (heading-text g (util/i->a idx) x 0 offset-t w)
       (+ x w))
     offset-l (map-indexed vector col-widths))
    (reposition)
    (.on viewport "moved" reposition)
    g))

(defn- grid [row-heights col-widths]
  (let [g (new pixi/Graphics)]
    (letfn [(grid-line*
              [sx sy ex ey]
              (grid-line g sx sy ex ey))
            (draw-horizontal [y] (grid-line* 0 y (:world-w styles/sizes) y))
            (draw-vertical [x] (grid-line* x 0 x (:world-h styles/sizes)))]
      (set! (.. g -position -x) (:heading-left-width styles/sizes))
      (set! (.. g -position -y) (:cell-h styles/sizes))
      (dorun (->> row-heights (reductions +) (map draw-horizontal)))
      (dorun (->> col-widths (reductions +) (map draw-vertical))))
    g))

(defn paint [{:keys [row-heights col-widths]} {:keys [pixi-app selections]}]
  (let [v (:viewport pixi-app)
        c (:container pixi-app)]
    (.removeChildren c)
    (.addChild c (grid row-heights col-widths))
    (.addChild c (top-heading col-widths v))
    (.addChild c (left-heading row-heights v))
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
        :screenHeight (.-offsetHeight (.getElementById js/document "canvas-container"))
        :screenWidth (.-offsetWidth (.getElementById js/document "canvas-container"))
        :worldHeight (:world-h styles/sizes)
        :worldWidth (:world-w styles/sizes)}))

(defn setup []
  (let [app (make-app)
        viewport (make-viewport app)]
    (.then
     (.load pixi/Assets "/fonts/SpaceGrotesk.fnt")
     #(rf/dispatch [::events/set-pixi-container app viewport (new pixi/Container)]))))

(defn- canvas* []
  (rc/create-class
   {:display-name "bean-canvas"
    :component-did-mount setup

    :reagent-render
    (fn [sheet ui]
      (when (:pixi-app ui) 
        (paint (:grid-dimensions sheet) ui)
        [:div]))}))

(defn canvas [] 
  [canvas* 
   @(rf/subscribe [::subs/sheet]) 
   @(rf/subscribe [::subs/canvas])])
