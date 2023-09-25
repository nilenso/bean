(ns bean.ui.sheet
  (:require [clojure.string :as str]
            [cljs.core :refer [char]]))

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

(defn cell [{:keys [set-mode edit-mode update-cell]} row col {:keys [mode error content representation] :as cell}]
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
                    (update-cell [row col] (.-textContent (.-currentTarget e))))
         :class (cs :bean-cell
                    (when (= mode :edit) :edit-mode)
                    (when error :cell-error))}
   (case mode
     :edit content
     representation)])

(defn i->a [i]
  (loop [a '()
         i (inc i)]
    (let [m (mod i 26)
          n (/ i 26)]
      (if (> n 1)
        (recur (cons (char (+ 64 m)) a)
               n)
        (cons (char (+ 64 m)) a)))))

(defn label-cell [i & [kind]]
  [:div {:key (str "label" i)
         :class (cs :bean-label
                    (when (= kind :alpha) :bean-label-top)
                    (when-not kind :bean-label-left)
                    (when (= i :bean) :bean-corner))}
   (case kind
     :alpha (i->a i)
     :bean ""
     (str (inc i)))])

(defn labels-top [rows]
  [:<>
   [label-cell :bean :bean]
   (map-indexed
    (fn [i _] ^{:key i}
      [label-cell i :alpha])
    (first rows))])

(defn row [state-fns i cells]
  [:<>
   [label-cell i]
   (map-indexed #(do ^{:key %1}
                  [cell state-fns i %1 %2])
                cells)])

(defn sheet1 [num-rows num-cols {:keys [grid]} state-fns]
  [:div {:class :bean-sheet
         :style {:grid-template-columns (str "var(--label-left-width)
                                              repeat(" num-cols ", var(--cell-width))")
                 :grid-template-rows (str "repeat(" (inc num-rows) ", var(--cell-height))")}}
   [labels-top grid]
   (map-indexed #(do ^{:key %1}
                  [row state-fns %1 %2]) grid)])
