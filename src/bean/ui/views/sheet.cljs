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
    (rf/dispatch [::events/clear-selections])
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

(defn- heading-text [^js g text x y h w]
  (let [bitmap (new
                pixi/BitmapText
                text
                #js {:fontName "SpaceGrotesk"
                     :tint (:heading-color styles/colors)
                     :fontSize (:heading-font-size styles/sizes)})]
    (->> (center-text! bitmap x y h w) (.addChild g))))

(defn- cell-text [^js g text x y h w error?]
  (let [bitmap (new pixi/BitmapText text
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
    (.beginFill mask 0xffffff)
    (.drawRect mask x y w h)
    (.endFill mask)
    (.addChild g mask)
    (set! (.-mask bitmap) mask)
    (.addChild g bitmap)))

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

(defn- heading-line [g sx sy ex ey]
  (native-line g (:heading-border styles/colors) sx sy ex ey))

(defn- col-resize-start [i ^js g col-widths viewport]
  (let [pos-fn #(.-x (.getLocalPosition ^js % g))
        col (px->index (- (pos-fn i) (/ (:resizer-handle styles/sizes) 2)) col-widths)
        start-x (index->px (inc col) col-widths)
        col-w (nth col-widths col)
        resizer-g (new pixi/Graphics)
        draw-resizers
        (fn [x]
          (.clear resizer-g)
          (col-resizer-line resizer-g (- start-x col-w))
          (col-resizer-line resizer-g x))
        on-drag-move #(draw-resizers (pos-fn %))
        on-drag-end
        (fn on-drag-end [i2]
          (.off viewport "pointermove" on-drag-move)
          (.off viewport "pointerup" on-drag-end)
          (.off viewport "pointerleave" on-drag-end)
          (let [x (pos-fn i2)
                distance (- x start-x)
                new-w (+ col-w distance)]
            (when (pos? new-w)
              (rf/dispatch [::events/resize-col col new-w]))
            (.clear resizer-g)))]
    (.addChild g resizer-g)
    (draw-resizers start-x)
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
    (.addChild g (col-resizers col-widths viewport))
    g))

(defn- grid-text [grid row-heights col-widths]
  (let [g (new pixi/Graphics)
        xs (reductions + 0 col-widths)
        ys (reductions + 0 row-heights)]
    (util/map-on-matrix-addressed
     (fn [[r c] cell]
       (let [text (:representation cell)]
         (when-not (empty? text)
           (cell-text
            g text
            (nth xs c) (nth ys r)
            (nth row-heights r) (nth col-widths c)
            (:error cell)))))
     grid)
    g))

(defn- draw-grid [row-heights col-widths]
  (let [g (new pixi/Graphics)]
    (letfn [(grid-line*
              [sx sy ex ey]
              (grid-line g sx sy ex ey))
            (draw-horizontal [y] (grid-line* 0 y (:world-w styles/sizes) y))
            (draw-vertical [x] (grid-line* x 0 x (:world-h styles/sizes)))]
      (set! (.. g -position -x) (:heading-left-width styles/sizes))
      (set! (.. g -position -y) (:cell-h styles/sizes))
      (dorun (->> row-heights (reductions +) (map draw-horizontal)))
      (dorun (->> col-widths (reductions +) (map draw-vertical)))
      (set! (.-eventMode g) "static")
      (set! (.-hitArea g) (new pixi/Rectangle 0 0 (:world-w styles/sizes) (:world-h styles/sizes)))
      (.on g "pointerdown" #(grid-pointer-down (.getLocalPosition ^js % g) row-heights col-widths)))
    g))

(defn paint [{:keys [grid grid-dimensions]} pixi-app]
  (let [{:keys [row-heights col-widths]} grid-dimensions
        v (:viewport pixi-app)
        c ^js (:container pixi-app)
        g ^js (draw-grid row-heights col-widths)]
    (.removeChildren c)
    (.addChild c g)
    (.addChild g (grid-text grid row-heights col-widths))
    (.addChild c (top-heading col-widths v))
    (.addChild c (left-heading row-heights v))
    (.addChild c (corner v))))

(defn- make-app []
  (new
   pixi/Application
   #js {:autoResize true
        :resizeTo (.getElementById js/document "grid-container")
        :resolution (.-devicePixelRatio js/window),
        :backgroundColor (:sheet-background styles/colors)
        :autoDensity true}))

(defn- make-viewport [app]
  (new
   pixi-viewport/Viewport
   #js {:events (.. app -renderer -events)
        :screenHeight (.-offsetHeight (.getElementById js/document "grid-container"))
        :screenWidth (.-offsetWidth (.getElementById js/document "grid-container"))
        :worldHeight (:world-h styles/sizes)
        :worldWidth (:world-w styles/sizes)}))

(defn- setup []
  (let [app (make-app)
        viewport (make-viewport app)]
    (.then
     ;; TODO: this delays the grid rendering by a bit
     (.load pixi/Assets "/fonts/SpaceGrotesk.fnt")
     #(rf/dispatch [::events/set-pixi-container app viewport (new pixi/Container)]))))

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

(defn- cell-input []
  (when-let [[r c] @(rf/subscribe [::subs/editing-cell])]
    (let [sheet (rf/subscribe [::subs/sheet])
          {:keys [row-heights col-widths]} (:grid-dimensions @sheet)
          cell (get-in @sheet [:grid r c])
          viewport (:viewport @(rf/subscribe [::subs/pixi-app]))
          transform-css #(input-transform-css [r c] viewport row-heights col-widths)
          reposition #(let [el (.getElementById js/document "cell-input")]
                        (set! (.. el -style -transform) (transform-css)))]
      (.on viewport "moved" reposition)
      (.on viewport "moved-end" reposition)
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
    :component-did-mount setup

    :component-did-update
    (fn [this _]
      (let [[sheet pixi-app] (rest (rc/argv this))]
        (when pixi-app
          (prn "Reconstructing graphics")
          (paint sheet pixi-app))))

    :reagent-render
    (fn []
      [:div])}))

(defn canvas []
  [canvas*
   @(rf/subscribe [::subs/sheet])
   @(rf/subscribe [::subs/pixi-app])])

(defn sheet []
  [:div {:id :grid-container}
   [canvas]
   [cell-input]])
