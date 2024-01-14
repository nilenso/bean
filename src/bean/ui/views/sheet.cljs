(ns bean.ui.views.sheet
  (:require [bean.ui.events :as events]
            [bean.ui.styles :as styles]
            [bean.ui.subs :as subs]
            [bean.ui.util :as util]
            [clojure.string :as string]
            [pixi-viewport :as pixi-viewport]
            [pixi.js :as pixi]
            [re-frame.core :as rf]
            [reagent.core :as rc]))

(defn- reset-listener! [name g event f pixi-app]
  (when-let [old-listener (get-in @pixi-app [:listeners name])]
    (.off g event old-listener))
  (.on g event f)
  (swap! pixi-app assoc-in [:listeners name] f))

(defn px->index [px offsets]
  (if (> px (reduce + offsets))
    -1
    (reduce
     (fn [{:keys [index current-pixel]} offset]
       (if (<= (+ current-pixel offset) px)
         {:index (inc index)
          :current-pixel (+ current-pixel offset)}
         (reduced index)))
     {:index 0 :current-pixel 0}
     offsets)))

(defn- index->px [index offsets]
  (apply + (take index offsets)))

(defn xy->rc [[x y] row-heights col-widths]
  [(px->index y row-heights) (px->index x col-widths)])

(defn rc->xy [[r c] row-heights col-widths]
  [(apply + (take c col-widths)) (apply + (take r row-heights))])

(defn- submit-cell-input []
  (when-let [el (.getElementById js/document "cell-input")]
    (rf/dispatch [::events/submit-cell-input (str (.-textContent el))])
    ;; Reagent does not clear the element when input moves to a blank cell.
    (set! (.-innerHTML el) nil)))

(defn- grid-pointer-down [interaction row-heights col-widths]
  (let [[r c] (xy->rc [(.-x interaction) (.-y interaction)] row-heights col-widths)]
    (submit-cell-input)
    (rf/dispatch [::events/clear-selection])
    (rf/dispatch [::events/start-selection [r c]])
    (rf/dispatch [::events/edit-cell [r c]])))

;; TODO: use the reframe keyboard library here
(defn- handle-cell-navigation [e [r c]]
  (let [move-to-cell (cond
                       (and (= (.-keyCode e) 13) (.-shiftKey e)) [(dec r) c]
                       (and (= (.-keyCode e) 9) (.-shiftKey e)) [r (dec c)]
                       (= (.-keyCode e) 13) [(inc r) c]
                       (= (.-keyCode e) 9) [r (inc c)])]
    (when move-to-cell
      (.preventDefault e)
      (submit-cell-input)
      (rf/dispatch [::events/edit-cell move-to-cell]))))

(defn- center-text! [bitmap-text x y h w]
  (let [text-h (.-height bitmap-text)
        text-w (.-width bitmap-text)]
    (set! (.-x bitmap-text) (- (+ x (/ w 2)) (/ text-w 2)))
    (set! (.-y bitmap-text) (- (+ y (/ h 2)) text-h)))
  bitmap-text)

(defn- heading-text [text x y h w]
  (let [g (new pixi/Graphics)
        bitmap (new
                pixi/BitmapText
                text
                #js {:fontName "SpaceGrotesk"
                     :tint (:heading-color styles/colors)
                     :fontSize (:heading-font-size styles/sizes)})
        mask (new pixi/Graphics)]
    (center-text! bitmap x y h w)
    (-> mask (.beginFill 0xffffff) (.drawRect x y w h) .endFill)
    (set! (.-mask bitmap) mask)
    (.addChild g mask)
    (.addChild g bitmap)
    g))

(defn- cell-text [text x y h w error?]
  (let [g (new pixi/Graphics)
        bitmap (new pixi/BitmapText text
                    #js {:fontName "SpaceGrotesk"
                         :tint (if error?
                                 (:cell-error-color styles/colors)
                                 (:cell-color styles/colors))
                         :fontSize (if error?
                                     (:error-font-size styles/sizes)
                                     (:cell-font-size styles/sizes))})
        mask (new pixi/Graphics)]
    (set! (.-x bitmap) (+ x (:cell-padding styles/sizes)))
    (set! (.-y bitmap) (+ y (:cell-padding styles/sizes)))
    (-> mask (.beginFill 0xffffff) (.drawRect x y w h) .endFill)
    (set! (.-mask bitmap) mask)
    (.addChild g mask)
    (.addChild g bitmap)
    g))

(defn- native-line [g color sx sy ex ey]
   ;; native lines can be 1px wide only.
  (.lineStyle g 1 color 1 0.5 true)
  (.moveTo g sx sy)
  (.lineTo g ex ey)
  (.lineStyle g 0 color 1 0.5 true))

(defn- grid-line [g sx sy ex ey]
  (native-line g (:grid-line styles/colors) sx sy ex ey))

(defn- col-resizer-line [g sx]
  (native-line g (:resizer-line styles/colors) sx 0 sx (:world-h styles/sizes)))

(defn- row-resizer-line [g sy]
  (native-line g (:resizer-line styles/colors) 0 sy (:world-w styles/sizes) sy))

(defn- heading-line [g sx sy ex ey]
  (native-line g (:heading-border styles/colors) sx sy ex ey))

(defn- row-resize-start [i ^js g row-heights viewport]
  (let [pos-fn #(.-y (.getLocalPosition ^js % g))
        row (px->index (- (pos-fn i) (/ (:resizer-handle styles/sizes) 2)) row-heights)
        start-y (index->px (inc row) row-heights)
        start-click-y (pos-fn i)
        row-h (nth row-heights row)
        resizer-g (new pixi/Graphics)
        draw-resizers
        (fn [y]
          (.clear resizer-g)
          (row-resizer-line resizer-g (- start-y row-h))
          (row-resizer-line resizer-g (+ start-y (- y start-click-y))))
        on-drag-move #(draw-resizers (pos-fn %))
        on-drag-end
        (fn on-drag-end [i2]
          (.off viewport "pointermove" on-drag-move)
          (.off viewport "pointerup" on-drag-end)
          (.off viewport "pointerleave" on-drag-end)
          (set! (.-interactiveChildren g) true)
          (let [y (pos-fn i2)
                distance (- y start-y)
                new-h (+ row-h distance)]
            (when (pos? new-h)
              (rf/dispatch [::events/resize-row row new-h]))
            (.destroy resizer-g)))]
    (set! (.-interactiveChildren g) false)
    (.addChild g resizer-g)
    (draw-resizers start-click-y)
    (.on viewport "pointermove" on-drag-move)
    (.on viewport "pointerup" on-drag-end)
    (.on viewport "pointerleave" on-drag-end)))

(defn- row-resizers [row-heights viewport]
  (let [g (new pixi/Graphics)]
    (dorun
     (map
      (fn [offset]
        (let [g2 (new pixi/Graphics)]
          (.addChild g g2)
          (set! (.-hitArea g2) (new pixi/Rectangle
                                    0
                                    (- offset (/ (:resizer-handle styles/sizes) 2))
                                    (:heading-left-width styles/sizes)
                                    (:resizer-handle styles/sizes)))))
      (reductions + row-heights)))
    (set! (.-eventMode g) "static")
    (set! (.-cursor g) "ns-resize")
    (set! (.. g -position -y) (:cell-h styles/sizes))
    (.on g "pointerdown" #(row-resize-start % g row-heights viewport))
    g))

(defn- col-resize-start [i ^js g col-widths viewport]
  (let [pos-fn #(.-x (.getLocalPosition ^js % g))
        col (px->index (- (pos-fn i) (/ (:resizer-handle styles/sizes) 2)) col-widths)
        start-x (index->px (inc col) col-widths)
        start-click-x (pos-fn i)
        col-w (nth col-widths col)
        resizer-g (new pixi/Graphics)
        draw-resizers
        (fn [x]
          (.clear resizer-g)
          (col-resizer-line resizer-g (- start-x col-w))
          (col-resizer-line resizer-g (+ start-x (- x start-click-x))))
        on-drag-move #(draw-resizers (pos-fn %))
        on-drag-end
        (fn on-drag-end [i2]
          (.off viewport "pointermove" on-drag-move)
          (.off viewport "pointerup" on-drag-end)
          (.off viewport "pointerleave" on-drag-end)
          (set! (.-interactiveChildren g) true)
          (let [x (pos-fn i2)
                distance (- x start-x)
                new-w (+ col-w distance)]
            (when (pos? new-w)
              (rf/dispatch [::events/resize-col col new-w]))
            (.destroy resizer-g)))]
    (set! (.-interactiveChildren g) false)
    (.addChild g resizer-g)
    (draw-resizers start-click-x)
    (.on viewport "pointermove" on-drag-move)
    (.on viewport "pointerup" on-drag-end)
    (.on viewport "pointerleave" on-drag-end)))

(defn- col-resizers [col-widths viewport]
  (let [g (new pixi/Graphics)]
    (dorun
     (map
      (fn [offset]
        (let [g2 (new pixi/Graphics)]
          (.addChild g g2)
          (set! (.-hitArea g2) (new pixi/Rectangle
                                    (- offset (/ (:resizer-handle styles/sizes) 2))
                                    0
                                    (:resizer-handle styles/sizes)
                                    (:cell-h styles/sizes)))))
      (reductions + col-widths)))
    (set! (.-eventMode g) "static")
    (set! (.-cursor g) "ew-resize")
    (set! (.. g -position -x) (:heading-left-width styles/sizes))
    (.on g "pointerdown" #(col-resize-start % g col-widths viewport))
    g))

(defn- draw-corner
  ([viewport]
   (let [g (new pixi/Graphics)
         reposition #(do (set! (.. g -position -x) (.-left viewport))
                         (set! (.. g -position -y) (.-top viewport)))]
     (.on viewport "moved" reposition)
     (reposition)
     (.beginFill g (:corner-background styles/colors))
     (.drawRect g 0 0 (:heading-left-width styles/sizes) (:cell-h styles/sizes))
     g))
  ([g _viewport] g))

(defn- draw-left-heading
  ([viewport]
   (let [g (new pixi/Graphics)
         reposition #(set! (.. g -position -x) (.-left viewport))]
     (.on viewport "moved" reposition)
     (reposition)
     g))
  ([^js g row-heights viewport]
   (let [offset-t (:cell-h styles/sizes)
         offset-l (:heading-left-width styles/sizes)]
     (.removeChildren g)
    ;; draw the background
     (.beginFill g (:heading-background styles/colors))
     (.drawRect g 0 0 offset-l (:world-h styles/sizes))
     (heading-line g offset-l 0 offset-l (:world-h styles/sizes))
    ;; draw individual heading borders
     (->> row-heights
          (reductions + offset-t)
          (map #(heading-line g 0 % offset-l %))
          dorun)
    ;; draw text
     (reduce
      (fn [y [idx w]]
        (.addChild g (heading-text (inc idx) 0 y w offset-l))
        (+ y w))
      offset-t (map-indexed vector row-heights))
     (.addChild g (row-resizers row-heights viewport))
     g)))

(defn- draw-top-heading
  ([viewport]
   (let [g (new pixi/Graphics)
         reposition #(set! (.. g -position -y) (.-top viewport))]
     (.on viewport "moved" reposition)
     (reposition)
     g))
  ([^js g col-widths viewport]
   (let [offset-t (:cell-h styles/sizes)
         offset-l (:heading-left-width styles/sizes)]
     (.removeChildren g)
     (.beginFill g (:heading-background styles/colors))
     (.drawRect g 0 0 (:world-w styles/sizes) offset-t)
     (heading-line g 0 offset-t (:world-w styles/sizes) offset-t)
     (->> col-widths
          (reductions + offset-l)
          (map #(heading-line g % 0 % offset-t))
          dorun)
     (reduce
      (fn [x [idx w]]
        (.addChild g (heading-text (util/i->a idx) x 0 offset-t w))
        (+ x w))
      offset-l (map-indexed vector col-widths))
     (.addChild g (col-resizers col-widths viewport))
     g)))

(defn- selection-rect* [g x y h w]
  (.beginFill g (:selection-fill styles/colors) (:selection-alpha styles/colors))
  (.lineStyle g (:selection-border styles/sizes) (:selection-border styles/colors) 1 1)
  (.drawRect g x y h w))

(defn- draw-selection
  ([] (new pixi/Graphics))
  ([g {:keys [start end]} row-heights col-widths]
   (let [[start-r start-c] start
         [end-r end-c] end
         [top-r top-c] [(min start-r end-r) (min start-c end-c)]
         [bottom-r bottom-c] [(max start-r end-r) (max start-c end-c)]]
     (.clear g)
     (set! (.-eventMode g) "none")
     (selection-rect*
      g
      (apply + (take top-c col-widths))
      (apply + (take top-r row-heights))
      (reduce + (subvec col-widths top-c (inc bottom-c)))
      (reduce + (subvec row-heights top-r (inc bottom-r))))
     g)))

(defn- draw-grid-text
  ([] (new pixi/Graphics))
  ([^js g grid row-heights col-widths]
   (.removeChildren g)
   (let [xs (reductions + 0 col-widths)
         ys (reductions + 0 row-heights)]
     (util/map-on-matrix-addressed
      (fn [[r c] cell]
        (let [text (:representation cell)]
          (when-not (empty? text)
            (.addChild g
                       (cell-text
                        text
                        (nth xs c) (nth ys r)
                        (nth row-heights r) (nth col-widths c)
                        (:error cell))))))
      grid)
     g)))

(defn- draw-grid
  ([]
   (let [g (new pixi/Graphics)]
     (set! (.-eventMode g) "static")
     (set! (.-hitArea g) (new pixi/Rectangle 0 0 (:world-w styles/sizes) (:world-h styles/sizes)))
     (set! (.. g -position -x) (:heading-left-width styles/sizes))
     (set! (.. g -position -y) (:cell-h styles/sizes))
     g))
  ([g row-heights col-widths pixi-app]
   (letfn [(grid-line*
             [sx sy ex ey]
             (grid-line g sx sy ex ey))
           (draw-horizontal [y] (grid-line* 0 y (:world-w styles/sizes) y))
           (draw-vertical [x] (grid-line* x 0 x (:world-h styles/sizes)))]
     (.clear g)
     (reset-listener!
      :grid-pointerdown g "pointerdown"
      #(grid-pointer-down (.getLocalPosition ^js % g) row-heights col-widths)
      pixi-app)
     (dorun (->> row-heights (reductions +) (map draw-horizontal)))
     (dorun (->> col-widths (reductions +) (map draw-vertical)))
     g)))

(defn- make-app []
  (let [app (new
             pixi/Application
             #js {:autoResize true
                  :resizeTo (.getElementById js/document "grid-container")
                  :resolution (.-devicePixelRatio js/window),
                  :backgroundColor (:sheet-background styles/colors)
                  :autoDensity true})]
    (.appendChild
     (.getElementById js/document "grid-container")
     (.-view app))
    (set! (.-__PIXI_APP__ js/globalThis) app)
    (.addEventListener js/window "wheel" #(.preventDefault %1) #js {:passive false})
    app))

(defn- make-viewport [app]
  (let [v (new
           pixi-viewport/Viewport
           #js {:events (.. app -renderer -events)
                :screenHeight (.-offsetHeight (.getElementById js/document "grid-container"))
                :screenWidth (.-offsetWidth (.getElementById js/document "grid-container"))
                :worldHeight (:world-h styles/sizes)
                :worldWidth (:world-w styles/sizes)})]
    (-> v
        (.clampZoom #js {:maxHeight 10000 :maxWidth 10000})
        (.drag #js {:clampWheel true :pressDrag false})
        (.wheel #js {:trackpadPinch true :wheelZoom false})
        (.clamp #js {:direction "all"}))))

(defn- make-container []
  (new pixi/Container))

(defn repaint [sheet selection pixi-app]
  (let [{:keys [row-heights col-widths]} (:grid-dimensions sheet)
        v (:viewport @pixi-app)]
    (draw-grid (:grid @pixi-app) row-heights col-widths pixi-app)
    (draw-grid-text (:grid-text @pixi-app) (:grid sheet) row-heights col-widths)
    (when selection (draw-selection (:selection @pixi-app) selection row-heights col-widths))
    (draw-top-heading (:top-heading @pixi-app) col-widths v)
    (draw-left-heading (:left-heading @pixi-app) row-heights v)
    (draw-corner (:corner @pixi-app) v)))

(defn setup [sheet pixi-app]
  (.then
   (.load pixi/Assets "/fonts/SpaceGrotesk.fnt")
   #(let [app (make-app)
          v (.addChild (.-stage app) (make-viewport app))
          c (.addChild v (make-container))
          grid (draw-grid)]
      (reset! pixi-app
              {:viewport v
               :container c
               :grid (.addChild c grid)
               :selection (.addChild grid (draw-selection))
               :grid-text (.addChild grid (draw-grid-text))
               :top-heading (.addChild c (draw-top-heading v))
               :left-heading (.addChild c (draw-left-heading v))
               :corner (.addChild c (draw-corner v))})
      (repaint sheet nil pixi-app))))

(defn- input-transform-css [rc ^js viewport row-heights col-widths]
  (let [offset-t (:cell-h styles/sizes)
        offset-l (:heading-left-width styles/sizes)
        world-transform (.-worldTransform viewport)
        [cell-x cell-y] (rc->xy rc row-heights col-widths)
        scaled-xy (.toScreen viewport
                             (+ offset-l cell-x)
                             (+ offset-t cell-y))
        over-headings? (or (< (.-x scaled-xy) offset-l)
                           (< (.-y scaled-xy) offset-t))]
    (str "matrix("
         (string/join "," [(if over-headings? 0
                               (.-a world-transform))
                           0 0
                           (.-d world-transform)
                           (.-x scaled-xy)
                           (.-y scaled-xy)])
         ")")))

(defn- cell-input [pixi-app]
  (when-let [[r c] @(rf/subscribe [::subs/editing-cell])]
    (let [sheet (rf/subscribe [::subs/sheet])
          {:keys [row-heights col-widths]} (:grid-dimensions @sheet)
          cell (get-in @sheet [:grid r c])
          viewport (:viewport @pixi-app)
          transform-css #(input-transform-css [r c] viewport row-heights col-widths)
          reposition #(let [el (.getElementById js/document "cell-input")]
                        (set! (.. el -style -transform) (transform-css)))]
      (reset-listener! :cell-input-reposition-move viewport "moved" reposition pixi-app)
      (reset-listener! :cell-input-reposition-move-end viewport "moved-end" reposition pixi-app)
      [:span {:id :cell-input
              :content-editable true
              :suppressContentEditableWarning true
              :spell-check false
              :style {:transform (transform-css)
                      :minHeight (nth row-heights r)
                      :minWidth (nth col-widths c)}
              :on-key-down #(handle-cell-navigation % [r c])}
       (:content cell)])))

(defn- canvas* []
  (rc/create-class
   {:display-name :grid-canvas
    :component-did-mount
    (fn [this]
      (let [[sheet _selection pixi-app] (rest (rc/argv this))]
        (prn "Setting up canvas")
        (setup sheet pixi-app)))

    :component-did-update
    (fn [this _]
      (let [[sheet selection pixi-app] (rest (rc/argv this))]
        (when @pixi-app
          (prn "Repaint canvas")
          (repaint sheet selection pixi-app))))

    :reagent-render
    (fn []
      [:div])}))

(defn canvas [pixi-app]
  [canvas*
   @(rf/subscribe [::subs/sheet])
   @(rf/subscribe [::subs/selection])
   pixi-app])

(defonce ^:private pixi-app* (atom nil))

(defn sheet []
  [:div {:id :grid-container}
   [canvas pixi-app*]
   [cell-input pixi-app*]])
