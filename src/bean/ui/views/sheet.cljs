(ns bean.ui.views.sheet
  (:require [bean.area :as area]
            [bean.grid :as grid]
            [bean.tables :as tables]
            [bean.ui.events :as events]
            [bean.ui.features :as features]
            [bean.ui.styles :as styles]
            [bean.ui.subs :as subs]
            [bean.ui.util :as util]
            [clojure.string :as string]
            [pixi-viewport :as pixi-viewport]
            [pixi.js :as pixi]
            [re-frame.core :as rf]
            [reagent.core :as rc]))

(defn- add-listener! [name g event f pixi-app]
  (.on g event f)
  (swap! pixi-app assoc-in [:listeners name]
         {:object g
          :event event
          :handler-fn f}))

(defn- remove-listener! [name pixi-app]
  (when-let [old-listener (get-in @pixi-app [:listeners name])]
    (let [{:keys [object event handler-fn]} old-listener]
      (.off object event handler-fn))))

(defn- reset-listener! [name g event f pixi-app]
  (remove-listener! name pixi-app)
  (add-listener! name g event f pixi-app))

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

(defn rcs->distance
  ([index1 index2 offsets]
   (apply + (subvec offsets index1 index2)))
  ([[top-r top-c] [bottom-r bottom-c] row-heights col-widths]
   [(rcs->distance top-c (inc bottom-c) col-widths)
    (rcs->distance top-r (inc bottom-r) row-heights)]))

(defn- area->xywh [{:keys [start end]} row-heights col-widths]
  (let [[x y] (rc->xy start row-heights col-widths)
        [w h] (rcs->distance start end row-heights col-widths)]
    [x y w h]))

(defn- i->point [^js i g]
  (.getLocalPosition i g))

(defn- i->rc [i g row-heights col-widths]
  (let [point (i->point i g)]
    (xy->rc [(.-x point) (.-y point)] row-heights col-widths)))

(defn- cell-h [r cell row-heights]
  (if-let [[end-r _] (get-in cell [:style :merged-until])]
    (rcs->distance r (inc end-r) row-heights)
    (nth row-heights r)))

(defn- cell-w [c cell col-widths]
  (if-let [[_ end-c] (get-in cell [:style :merged-until])]
    (rcs->distance c (inc end-c) col-widths)
    (nth col-widths c)))

(defn- submit-cell-input []
  (when-let [el (.getElementById js/document "cell-input")]
    (rf/dispatch [::events/submit-cell-input (str (.-textContent el))])
    ;; Reagent does not clear the element when input moves to a blank cell.
    (set! (.-innerHTML el) nil)))

(defn- selection->rect [^js g area row-heights col-widths]
  (when (:start area)
    (let [[x y w h] (area->xywh area row-heights col-widths)
          color (:selection styles/colors)]
      (.beginFill g color (:selection-alpha styles/colors))
      (.lineStyle g (:selection-border styles/sizes) color 1 1)
      (.drawRect g x y w h))))

(defn- edit-cell [rc sheet]
  (rf/dispatch-sync [::events/select-table (tables/cell-table rc sheet)])
  (rf/dispatch [::events/edit-cell rc]))

(defn- grid-selection-end [area pixi-app]
  (remove-listener! :grid-selection-move pixi-app)
  (remove-listener! :grid-selection-up pixi-app)
  (remove-listener! :grid-selection-up-outside pixi-app)
  (rf/dispatch-sync [::events/set-selection area]))

(defn- grid-selection-move [area row-heights col-widths pixi-app]
  (.clear (:selection @pixi-app))
  (selection->rect (:selection @pixi-app) area row-heights col-widths))

(defn- grid-selection-start [start grid-g row-heights col-widths pixi-app]
  (let [i->area #(let [rc (i->rc % grid-g row-heights col-widths)]
                   (area/bounds->area start rc))]
    (reset-listener!
     :grid-selection-move grid-g "globalpointermove"
     #(grid-selection-move (i->area %) row-heights col-widths pixi-app)
     pixi-app)
    (reset-listener!
     :grid-selection-up grid-g "pointerup"
     #(grid-selection-end (i->area %) pixi-app)
     pixi-app)
    (reset-listener!
     :grid-selection-up-outside grid-g "pointerupoutside"
     #(grid-selection-end (i->area %) pixi-app)
     pixi-app)))

(defn- cell-pointer-down [rc grid-g sheet row-heights col-widths pixi-app]
  (submit-cell-input)
  (rf/dispatch [::events/clear-edit-cell])
  (edit-cell rc sheet)
  (grid-selection-start
   (util/merged-or-self rc sheet)
   grid-g row-heights col-widths pixi-app))

(defn- grid-pointer-down [i grid-g sheet row-heights col-widths pixi-app]
  (let [rc (i->rc i grid-g row-heights col-widths)]
    (cell-pointer-down rc grid-g sheet row-heights col-widths pixi-app)))

;; TODO: use the reframe keyboard library here
(defn- handle-cell-navigation [e [r c] sheet]
  (let [[mr mc] (get-in sheet [:grid r c :style :merged-until])
        move-to-cell (cond
                       (and (= (.-keyCode e) 13) (.-shiftKey e)) [(dec r) c]
                       (and (= (.-keyCode e) 9) (.-shiftKey e)) [r (dec c)]
                       (= (.-keyCode e) 13) [(if mr (inc mr) (inc r)) c]
                       (= (.-keyCode e) 9) [r (if mc (inc mc) (inc c))])
        [move-to-r move-to-c] move-to-cell]
    (when (and (nat-int? move-to-r) (nat-int? move-to-c))
      (.preventDefault e)
      (submit-cell-input)
      (edit-cell move-to-cell sheet))))

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

(defn- cell-text [cell x y h w]
  (let [g (new pixi/Graphics)
        text (:representation cell)
        bold? (get-in cell [:style :bold])
        error? (:error cell)
        merged? (get-in cell [:style :merged-until])
        bitmap (new pixi/BitmapText text
                    #js {:fontName (if bold? "SpaceGrotesk-Bold" "SpaceGrotesk")
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
    (when merged? (center-text! bitmap x y h w))
    (.addChild g mask)
    (.addChild g bitmap)
    g))

(defn- native-line [g color alpha sx sy ex ey]
   ;; native lines can be 1px wide only.
  (.lineStyle g 1 color alpha 0.5 true)
  (.moveTo g sx sy)
  (.lineTo g ex ey)
  (.lineStyle g 0 color alpha 0.5 true))

(defn- grid-line [g sx sy ex ey]
  (native-line g (:grid-line styles/colors) 0.2 sx sy ex ey))

(defn- col-resizer-line [g sx]
  (native-line g (:resizer-line styles/colors) 1 sx 0 sx (:world-h styles/sizes)))

(defn- row-resizer-line [g sy]
  (native-line g (:resizer-line styles/colors) 1 0 sy (:world-w styles/sizes) sy))

(defn- heading-line [g sx sy ex ey]
  (native-line g (:heading-border styles/colors) 1 sx sy ex ey))

(defn- row-resize-start [i ^js g row-heights viewport]
  (let [pos-fn #(.-y (i->point % g))
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
  (let [pos-fn #(.-x (i->point % g))
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

(defn- draw-merged-cells
  ([]
   (let [g (new pixi/Graphics)] g))
  ([^js g {:keys [grid]} row-heights col-widths]
   (.clear g)
   (util/map-on-matrix-addressed
    (fn [rc cell]
      (when-let [merged-until (get-in cell [:style :merged-until])]
        (let [[x y w h] (area->xywh {:start rc :end merged-until} row-heights col-widths)]
          (.beginFill g (or (get-in cell [:style :background]) 0xffffff) 1)
          (.drawRect g (+ x 0.25) (+ y 0.25) (- w 0.5) (- h 0.5)))))
    grid)
   g))

(defn- draw-table-highlight [^js g table-name x y w h & [hover?]]
  (let [font-size (:table-name-font styles/sizes)
        text-bitmap (new pixi/BitmapText table-name
                         #js {:fontName "SpaceGrotesk"
                              :tint (:table-name styles/colors)
                              :fontSize font-size})
        color (if hover?
                (:table-highlight-hover styles/colors)
                (:table-highlight styles/colors))
        padding (:table-name-padding styles/sizes)
        padded #(+ (* 2 padding) %)]
    (.lineStyle g (:table-highlight styles/sizes) color 1 0.5)
    (.drawRect g x y w h)
    (.beginFill g color 1)
    (.drawRect g x
               (- y (padded font-size))
               (padded (.-width text-bitmap))
               (padded font-size))
    (.beginFill g 0x0000000 0)
    (set! (.-x text-bitmap) (+ x padding))
    (set! (.-y text-bitmap) (- y (padded font-size)))
    (.addChild g text-bitmap)
    g))

(defn- button! [sprite ^js g x y w on-click]
  (set! (.-eventMode sprite) "static")
  (set! (.-cursor sprite) "pointer")
  (.on sprite "pointerdown" #(do
                               (on-click)
                               (.stopPropagation %1)))
  (set! (.-x sprite) x)
  (set! (.-y sprite) y)
  (set! (.-width sprite) w)
  (set! (.-height sprite) (* (/ w (.. sprite -texture -baseTexture -width))
                             (.. sprite -texture -baseTexture -height)))
  (.addChild g sprite)
  sprite)

(defn- mark-skip-cells [table-name selection]
  (rf/dispatch [::events/clear-edit-cell])
  (rf/dispatch [::events/mark-skip-cells table-name
                (area/area->addresses selection)]))

(defn- remove-label [table-name selection]
  (rf/dispatch [::events/clear-edit-cell])
  (rf/dispatch [::events/remove-labels table-name
                (area/area->addresses selection)]))

(defn- add-label [table-name selection dirn]
  (rf/dispatch [::events/clear-edit-cell])
  (rf/dispatch [::events/add-labels table-name
                (area/area->addresses selection) dirn]))

(defn- draw-label-controls [icons table-name selection]
  (let [g (new pixi/Graphics)]
    (.lineStyle g 2 0xcccccc 1 0.5)
    (.beginFill g 0xffffff)
    (.drawRoundedRect g 0 0 30 112 5)
    (set! (.-eventMode g) "static")
    (.on g "pointerdown" #(.stopPropagation %))
    (button! (new pixi/Sprite (:add-top-label icons))
             g 5 5 20
             #(add-label table-name selection :top))
    (button! (new pixi/Sprite (:add-left-label icons))
             g 5 30 20
             #(add-label table-name selection :left))
    (button! (new pixi/Sprite (:add-skip-label icons))
             g 5 55 20
             #(mark-skip-cells table-name selection))
    (button! (new pixi/Sprite (:trash-label icons))
             g 5 80 20
             #(remove-label table-name selection))
    g))

(defn- draw-skipped-cells [^js g textures sheet skipped-cells row-heights col-widths xs ys]
  (doseq [[r c] skipped-cells]
    (let [bg (new pixi/TilingSprite (:stripes textures)
                  (cell-w c (get-in sheet [:grid r c]) col-widths)
                  (cell-h r (get-in sheet [:grid r c]) row-heights))]
      (set! (.-x bg) (nth xs c))
      (set! (.-y bg) (nth ys r))
      (set! (.-alpha bg) 0.05)
      (set! (.. bg -tileScale -x) 0.7)
      (set! (.. bg -tileScale -y) 1.7)
      (.addChild g bg))))

(defn- draw-label-bounds [textures sheet table-name labels row-heights col-widths]
  (let [xs (reductions + 0 col-widths)
        ys (reductions + 0 row-heights)
        g (new pixi/Graphics)]
    (let [skipped-cells (tables/skipped-cells sheet table-name)]
      (doseq [[[label-r label-c :as label] {:keys [color dirn]}] labels]
        (doseq [[r c] (tables/label->cells sheet table-name label)]
          (if (get skipped-cells [r c])
            (.beginFill g color 0.2)
            (.beginFill g color 0.5))
          (case dirn
            :top (when (= c label-c)
                   (.drawRect g
                              (+ (nth xs c) (* label-c 1.5)) (nth ys r)
                              2 (nth row-heights r)))
            :left (when (= r label-r)
                    (.drawRect g
                               (nth xs c) (+ (nth ys r) label-r)
                               (nth col-widths c) 4))))
        (let [[r c] label]
          (.beginFill g color 0.25)
          (.drawRect g
                     (nth xs c) (nth ys r)
                     (cell-w c (get-in sheet [:grid r c]) col-widths)
                     (cell-h r (get-in sheet [:grid r c]) row-heights))))
      (draw-skipped-cells g textures sheet skipped-cells row-heights col-widths xs ys)
      (.endFill g))
    g))

(defn- draw-tables
  ([] (new pixi/Graphics))
  ([^js g textures {:keys [tables] :as sheet} {:keys [selection selected-table]} row-heights col-widths]
   (-> g (.clear) (.removeChildren))
   (doseq [[table-name table-data] tables]
     (let [[x y w h] (area->xywh table-data row-heights col-widths)
           border (new pixi/Graphics)
           highlight (new pixi/Graphics)
           highlight-on-hover
           (fn []
             (.on border "pointerover"
                  #(draw-table-highlight highlight table-name x y w h true))
             (.on border "pointerout"
                  #(-> highlight (.clear) (.removeChildren))))
           extra-hitarea-y (+ (* 2 (:table-name-padding styles/sizes))
                              (:table-name-font styles/sizes))]
       (-> g (.addChild border) (.addChild highlight))
       (set! (.-eventMode border) "static")
       (set! (.-hitArea border) (new pixi/Rectangle
                                     x (- y extra-hitarea-y)
                                     w (+ h extra-hitarea-y)))
       (.lineStyle border (:table-border styles/sizes) (:table-border styles/colors) 1 0.5)
       (.drawRect border x y w h)

       (if (= selected-table table-name)
         (let [label-controls (draw-label-controls textures table-name selection)]
           (draw-table-highlight highlight table-name x y w h)
           (.addChild g (draw-label-bounds textures sheet table-name (:labels table-data) row-heights col-widths))
           (.addChild g label-controls)
           (set! (.-x label-controls) (+ x w 5))
           (set! (.-y label-controls) y))
         (highlight-on-hover))))))

(defn- draw-cell-backgrounds
  ([] (let [g (new pixi/Graphics)]
        (set! (.. g -position -x) (:heading-left-width styles/sizes))
        (set! (.. g -position -y) (:cell-h styles/sizes))
        (set! (.-interactiveChildren g) false)
        g))
  ([^js g {:keys [grid]} row-heights col-widths]
   (.clear g)
   (let [xs (reductions + 0 col-widths)
         ys (reductions + 0 row-heights)]
     (util/map-on-matrix-addressed
      (fn [[r c] cell]
        (when-let [background (get-in cell [:style :background])]
          (.beginFill g background 1)
          (.drawRect g
                     (nth xs c) (nth ys r)
                     (nth col-widths c) (nth row-heights r))))
      grid)
     g)))

(defn- draw-corner
  ([viewport]
   (let [g (new pixi/Graphics)
         reposition #(do (set! (.. g -position -x) (.-left viewport))
                         (set! (.. g -position -y) (.-top viewport)))]
     (.on viewport "moved" reposition)
     (reposition)
     (.beginFill g (:corner-background styles/colors))
     (.drawRect g 0 0 (:heading-left-width styles/sizes) (:cell-h styles/sizes))
     (.lineStyle g (:heading-border styles/sizes) (:heading-border styles/colors) 1 0)
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
     (.lineStyle g (:heading-border styles/sizes) (:heading-border styles/colors) 1 0)
     (.drawRect g 0 0 offset-l (:world-h styles/sizes))
    ;; draw individual heading borders
     (->> row-heights
          (reductions + offset-t)
          (drop 1)
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
     (.lineStyle g (:heading-border styles/sizes) (:heading-border styles/colors) 1 0)
     (.drawRect g 0 0 (:world-w styles/sizes) offset-t)
     (->> col-widths
          (reductions + offset-l)
          (drop 1)
          (map #(heading-line g % 0 % offset-t))
          dorun)
     (reduce
      (fn [x [idx w]]
        (.addChild g (heading-text (util/i->a idx) x 0 offset-t w))
        (+ x w))
      offset-l (map-indexed vector col-widths))
     (.addChild g (col-resizers col-widths viewport))
     g)))

(defn- draw-selection
  ([]
   (let [g (new pixi/Graphics)]
     (set! (.-eventMode g) "none")
     g))
  ([g selection row-heights col-widths]
   (.clear g)
   (when selection
     (selection->rect g selection row-heights col-widths))))

(defn- draw-cell-text
  ([] (new pixi/Graphics))
  ([^js g {:keys [grid]} row-heights col-widths]
   (.removeChildren g)
   (let [xs (reductions + 0 col-widths)
         ys (reductions + 0 row-heights)]
     (util/map-on-matrix-addressed
      (fn [[r c] cell]
        (let [text (:representation cell)]
          (when-not (empty? text)
            (.addChild g (cell-text
                          cell
                          (nth xs c) (nth ys r)
                          (cell-h r cell row-heights)
                          (cell-w c cell col-widths))))))
      grid)
     g)))

(defn- draw-spills
  ([] (new pixi/Graphics))
  ([^js g {:keys [grid]} row-heights col-widths]
   (.clear g)
   (.lineStyle g 1 0x006666 0.5 1)
   (util/map-on-matrix-addressed
    (fn [_ cell]
      (when-let [spills (:spilled-into cell)]
        (let [[x y w h] (area->xywh
                         (area/addresses->area spills)
                         row-heights col-widths)]
          (.drawRect g x y w h))))
    grid)
   g))

(defn- draw-grid
  ([]
   (let [g (new pixi/Graphics)]
     (set! (.-eventMode g) "static")
     (set! (.-hitArea g) (new pixi/Rectangle 0 0 (:world-w styles/sizes) (:world-h styles/sizes)))
     (set! (.. g -position -x) (:heading-left-width styles/sizes))
     (set! (.. g -position -y) (:cell-h styles/sizes))
     g))
  ([g sheet row-heights col-widths pixi-app]
   (letfn [(grid-line*
             [sx sy ex ey]
             (grid-line g sx sy ex ey))
           (draw-horizontal [y] (grid-line* 0 y (:world-w styles/sizes) y))
           (draw-vertical [x] (grid-line* x 0 x (:world-h styles/sizes)))]
     (.clear g)
     (reset-listener!
      :grid-pointerdown g "pointerdown"
      #(grid-pointer-down % g sheet row-heights col-widths pixi-app)
      pixi-app)
     (dorun (->> row-heights (reductions +) (map draw-horizontal)))
     (dorun (->> col-widths (reductions +) (map draw-vertical)))
     g)))

(defn- make-app []
  (let [app (new
             pixi/Application
             #js {:autoResize true
                  :resizeTo (.getElementById js/document "grid-container")
                  :resolution (.-devicePixelRatio js/window)
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

(defn- make-fonts-then [cb]
  (if-not (.get pixi/Assets "SpaceGrotesk")
    (do
      (.addBundle pixi/Assets "fonts" #js {"SpaceGrotesk" "/fonts/SpaceGrotesk.fnt"
                                           "SpaceGrotesk-Bold" "/fonts/SpaceGrotesk-Bold.fnt"})
      (.then (.loadBundle pixi/Assets "fonts") cb))
    (cb)))

(defn repaint [sheet {grid-ui :grid} pixi-app]
  (let [{:keys [row-heights col-widths]} (:grid-dimensions sheet)
        v (:viewport @pixi-app)]
    (draw-grid (:grid @pixi-app) sheet row-heights col-widths pixi-app)
    (draw-spills (:spills @pixi-app) sheet row-heights col-widths)
    (draw-merged-cells (:merged-cells @pixi-app) sheet row-heights col-widths)
    (draw-cell-backgrounds (:cell-backgrounds @pixi-app) sheet row-heights col-widths)
    (draw-cell-text (:cell-text @pixi-app) sheet row-heights col-widths)
    (draw-tables (:tables @pixi-app) (:textures @pixi-app) sheet grid-ui row-heights col-widths)
    (draw-selection (:selection @pixi-app) (:selection grid-ui) row-heights col-widths)
    (draw-top-heading (:top-heading @pixi-app) col-widths v)
    (draw-left-heading (:left-heading @pixi-app) row-heights v)
    (draw-corner (:corner @pixi-app) v)))

(defn setup [sheet ui pixi-app]
  (make-fonts-then
   #(let [app (make-app)
          v (.addChild (.-stage app) (make-viewport app))
          c (.addChild v (make-container))
          cell-background (.addChild c (draw-cell-backgrounds))
          grid (.addChild c (draw-grid))
          spills (.addChild grid (draw-spills))
          merged-cells (.addChild grid (draw-merged-cells))
          cell-text (.addChild grid (draw-cell-text))
          tables (.addChild grid (draw-tables))
          selection (.addChild grid (draw-selection))
          top-heading (.addChild c (draw-top-heading v))
          left-heading (.addChild c (draw-left-heading v))
          corner (.addChild c (draw-corner v))]
      (reset! pixi-app
              {:viewport v
               :container c
               :grid grid
               :spills spills
               :selection selection
               :tables tables
               :merged-cells merged-cells
               :cell-backgrounds cell-background
               :cell-text cell-text
               :top-heading top-heading
               :left-heading left-heading
               :corner corner
               :textures {:add-top-label (.from pixi/Texture "/img/top-label.png")
                          :add-left-label (.from pixi/Texture "/img/left-label.png")
                          :add-skip-label (.from pixi/Texture "/img/skip-label.png")
                          :stripes (.from pixi/Texture "/img/stripes.jpg")
                          :trash-label (.from pixi/Texture "/img/trash-label.png")}})
      (repaint sheet ui pixi-app))))

(defn- input-transform-css [rc ^js viewport row-heights col-widths]
  (let [offset-t (:cell-h styles/sizes)
        offset-l (:heading-left-width styles/sizes)
        world-transform (.-worldTransform viewport)
        [cell-x cell-y] (rc->xy rc row-heights col-widths)
        scaled-xy (.toScreen viewport
                             (+ offset-l cell-x)
                             (+ offset-t cell-y))
        over-headings? (or (< (.-x scaled-xy) (- offset-l 10))
                           (< (.-y scaled-xy) (- offset-t 10)))]
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
          reposition #(when-let [el (.getElementById js/document "cell-input")]
                        (set! (.. el -style -transform) (transform-css)))
          background (get-in cell [:style :background])
          bold? (get-in cell [:style :bold])
          merged? (get-in cell [:style :merged-until])]
      (reset-listener! :cell-input-reposition-move viewport "moved" reposition pixi-app)
      (reset-listener! :cell-input-reposition-move-end viewport "moved-end" reposition pixi-app)
      [:span {:id :cell-input
              :content-editable true
              :suppressContentEditableWarning true
              :spell-check false
              :auto-focus true
              :style {:transform (transform-css)
                      :minHeight (cell-h r cell row-heights)
                      :minWidth (cell-w c cell col-widths)
                      :text-align (when merged? :center)
                      :line-height (str (- (cell-h r cell row-heights) 12) "px")
                      :background-color (when background (util/color-int->hex background))
                      :fontWeight (if bold? "bold" "normal")}
              :on-pointer-down #(grid-selection-start
                                 (util/merged-or-self [r c] sheet)
                                 (:grid @pixi-app) row-heights col-widths pixi-app)
              :on-pointer-up #(let [canvas (.querySelector js/document "#grid-container canvas")]
                                (.dispatchEvent canvas (new js/MouseEvent "pointerup" %)))
              :on-key-down #(handle-cell-navigation % [r c] @sheet)}
       (:content cell)])))

(defn- canvas* []
  (rc/create-class
   {:display-name :grid-canvas
    :component-did-mount
    (fn [this]
      (let [[sheet ui pixi-app] (rest (rc/argv this))]
        (prn "Setting up canvas")
        (setup sheet ui pixi-app)))

    :component-did-update
    (fn [this _]
      (let [[sheet ui pixi-app] (rest (rc/argv this))]
        (when @pixi-app
          (prn "Repaint canvas")
          (repaint sheet ui pixi-app))))

    :component-will-unmount
    (fn [this]
      (let [[_ _ pixi-app] (rest (rc/argv this))]
        (.destroy (:viewport @pixi-app))))

    :reagent-render
    (fn [])}))

(defn canvas [pixi-app]
  [canvas*
   @(rf/subscribe [::subs/sheet])
   @(rf/subscribe [::subs/ui])
   pixi-app])

(defn controls []
  (let [selection @(rf/subscribe [::subs/selection])
        sheet @(rf/subscribe [::subs/sheet])]
    [:div {:class :controls-container}
     [:button {:class [:controls-btn
                       (when (grid/all-bold? sheet (area/area->addresses selection))
                         :pressed)]
               :on-click #(rf/dispatch [::events/toggle-cell-bold
                                        (area/area->addresses selection)])}
      "B"]
     [:button {:class [:controls-btn]
               :on-click #(rf/dispatch [::events/merge-cells selection])
               :disabled (not (grid/can-merge? sheet (area/area->addresses selection)))}
      "Merge"]
     [:button {:class [:controls-btn]
               :on-click #(rf/dispatch [::events/unmerge-cells (area/area->addresses selection) selection])
               :disabled (not (grid/can-unmerge? sheet (area/area->addresses selection)))}
      "Unmerge"]
     [:div {:class :controls-background-buttons}
      (for [color styles/cell-background-colors]
        [:button {:class :set-background-btn
                  :key (or color "transparent")
                  :style {:background-color (if color
                                              (str "#" (.toString color 16))
                                              "transparent")}
                  :on-mouse-down #(when selection
                                    (rf/dispatch [::events/set-cell-backgrounds
                                                  (area/area->addresses selection)
                                                  color]))} ""])]]))

(defonce ^:private pixi-app* (atom nil))

(defn sheet []
  [:div {:class :sheet-container}
   [controls]
   [:div {:id :grid-container}
    [canvas pixi-app*]
    [cell-input pixi-app*]]])
