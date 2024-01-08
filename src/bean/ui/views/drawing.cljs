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

(defn- submit-cell-input []
  (when-let [el (.getElementById js/document "cell-input")]
    (rf/dispatch [::events/submit-cell-input (str (.-textContent el))])
    ;; Reagent does not clear the element when input moves to a blank cell.
    (set! (.-innerHTML el) nil)))

(defn on-mouse-down [interaction row-heights col-widths]
  (let [[r c] (xy->rc [(.-x interaction) (.-y interaction)] row-heights col-widths)]
    (submit-cell-input)
    (rf/dispatch [::events/clear-selections])
    (rf/dispatch [::events/start-selection [r c]])
    (rf/dispatch [::events/edit-cell [r c]])))

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

(defn- heading-text [g text x y h w]
  (let [bitmap (new
                pixi/BitmapText
                text
                #js {:fontName "SpaceGrotesk"
                     :tint (:heading-color styles/colors)
                     :fontSize (:heading-font-size styles/sizes)})]
    (->> (center-text! bitmap x y h w) (.addChild g))))

(defn- cell-text [g text x y h w]
  (let [bitmap (new pixi/BitmapText text
                    #js {:fontName "SpaceGrotesk"
                         :tint (:cell-color styles/colors)
                         :fontSize 14})
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
            (nth row-heights r) (nth col-widths c)))))
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
      (.on g "pointerdown" #(on-mouse-down (.getLocalPosition % g) row-heights col-widths)))
    g))

(defn paint [{:keys [grid grid-dimensions]} pixi-app]
  (let [{:keys [row-heights col-widths]} grid-dimensions
        v (:viewport pixi-app)
        c (:container pixi-app)
        g (draw-grid row-heights col-widths)]
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

(defn setup []
  (let [app (make-app)
        viewport (make-viewport app)]
    (.then
     (.load pixi/Assets "/fonts/SpaceGrotesk.fnt")
     #(rf/dispatch [::events/set-pixi-container app viewport (new pixi/Container)]))))

(defn cell-input []
  (when-let [[r c] @(rf/subscribe [::subs/editing-cell])]
    (let [sheet (rf/subscribe [::subs/sheet])
          {:keys [row-heights col-widths]} (:grid-dimensions @sheet)
          cell (get-in @sheet [:grid r c])
          offset-t (:cell-h styles/sizes)
          offset-l (:heading-left-width styles/sizes)]
      [:span {:id :cell-input
              :content-editable true
              :suppressContentEditableWarning true
              :spell-check false
              :style {:top (+ offset-t (apply + (take r row-heights)))
                      :left (+ offset-l (apply + (take c col-widths)))
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
