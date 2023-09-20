(ns bean.ui.main
  (:require [reagent.dom :as r]
            [reagent.core :as rc]
            [bean.grid :as grid]
            [bean.ui.sheet :as sheet]))

(def sheet (rc/atom nil))
(def selected-cell (rc/atom nil))

(defn- start-sheet []
  (vec
   (for [row-num (range 20)]
     (vec (map (fn [_] "") (range 12))))))

(defn update-cell [address content]
  (swap! sheet #(grid/eval-sheet % address content)))

(defn cell-selector []
  (when @selected-cell
   [:div
    {:id :cell-selector
     :style {:top (str (+ 30 (* (first @selected-cell) 30)) "px")
             :left (str (+ 40 (* (second @selected-cell) 110)) "px")
             :display :block}}]))

(defn set-mode [[r c] mode]
  (swap! sheet #(update-in % [:grid r c :mode] (constantly mode)))
  (when (= mode :edit) (reset! selected-cell [r c])))

(defn active-sheet []
  [sheet/sheet1 @sheet {:update-cell update-cell
                        :set-mode set-mode
                        :edit-mode #(set-mode % :edit)}])

(defn ^:export main []
  (reset! sheet (grid/eval-sheet (start-sheet)))
  (r/render
   [:div
    [:div {:class :container}
     [cell-selector]
     [active-sheet]]]
   (.getElementById js/document "app")))
