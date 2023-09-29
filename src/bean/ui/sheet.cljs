(ns bean.ui.sheet
  (:require [clojure.string :as str]
            [cljs.core :refer [char]]
            [bean.ui.drawing :as drawing]
            [bean.ui.util :refer [px]]))

(defn cs [& classes]
  (->> classes
       (remove nil?)
       (map name)
       (str/join " ")))

(defn- cell-dom-el
  [[row col]]
  (.querySelector
   js/document
   (str "[data-row=\"" row "\"][data-col=\"" col "\"]")))

(defn- sizes->pxs [sizes]
  (reduce #(str %1 (px %2) " ") "" sizes))

(defn- cell [{:keys [set-mode edit-mode update-cell]} row col {:keys [mode error content representation] :as cell}]
  [:div {:content-editable true
         :suppressContentEditableWarning true
         :data-row row
         :data-col col
         :on-focus (fn [_] (edit-mode [row col])) ; Relies on edit mode getting reset on grid evaluation
         :on-key-down #(when (= (.-keyCode %) 13)
                         (.preventDefault %)
                         (-> % .-target .blur)
                         (let [below [(inc row) col]]
                           (.focus (cell-dom-el below))
                           (edit-mode below)))
         :on-blur (fn [e]
                    (set-mode [row col] :view)
                    (let [input (.-textContent (.-target e))]
                      (set! (.-innerHTML (.-target e)) (:value cell))
                      (update-cell [row col] input)))
         :class (cs :bean-cell
                    (when (= mode :edit) :edit-mode)
                    (when error :cell-error))}
   (case mode
     :edit content
     representation)])

(defn- i->a [i]
  (loop [a '()
         i (inc i)]
    (let [m (mod i 26)
          n (/ i 26)]
      (if (> n 1)
        (recur (cons (char (+ 64 m)) a)
               n)
        (cons (char (+ 64 m)) a)))))

(defn- labels-top [rows state-fns]
  [:<>
   [:div {:class (cs :bean-label :bean-corner)}]
   (map-indexed
    (fn [i _] ^{:key i}
      [:div {:key (str "label" i)
             :data-col i
             :class (cs :bean-label :bean-label-top)
             :on-mouse-down #(drawing/resize-top %1 (:resize-col state-fns))}
       (i->a i)])
    (first rows))])

(defn- row [state-fns i cells]
  [:<>
   [:div {:key (str "label" i)
          :data-row i
          :class (cs :bean-label :bean-label-left)
          :on-mouse-down #(drawing/resize-left %1 (:resize-row state-fns))}
    (inc i)]
   (map-indexed #(do ^{:key %1}
                  [cell state-fns i %1 %2])
                cells)])

(defn cell-selector [{:keys [selected-cell row-heights col-widths]}]
  (let [[r c] selected-cell]
    [:div
     {:id :cell-selector
      :style {:top (px (+ 30 (apply + (take r row-heights))))
              :left (px (+ 40 (apply + (take c col-widths))))
              :display :block
              :height (get row-heights r)
              :width (get col-widths c)}}]))

(defn sheet [{:keys [grid ui]} state-fns]
  [:div
   {:class :bean-sheet
    :id :bean-sheet
    :style {:grid-template-columns (str
                                    "var(--label-left-width) "
                                    (sizes->pxs (:col-widths ui)))
            :grid-template-rows (str
                                 "var(--cell-height) "
                                 (sizes->pxs (:row-heights ui)))}}
   [labels-top grid state-fns]
   (map-indexed #(do ^{:key %1}
                  [row state-fns %1 %2]) grid)
   [:div {:id :bean-resize-indicator-v}]
   [:div {:id :bean-resize-indicator-h}]
   (when (:selected-cell ui) [cell-selector ui])])
