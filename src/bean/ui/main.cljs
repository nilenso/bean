(ns bean.ui.main
  (:require [reagent.dom :as r]
            [reagent.core :as rc]
            [bean.grid :as grid]
            [bean.ui.sheet :as sheet]))

(def sheet (rc/atom nil))

(defn- start-sheet []
  (vec
   (for [row-num (range 20)]
     (vec (map (fn [_] "") (range 12))))))

(defn update-cell [address content]
  (swap! sheet
         #(grid/eval-sheet % address content)))

(defn set-mode [[r c] mode]
  (swap! sheet #(update-in % [:grid r c :mode] (constantly mode))))

(defn active-sheet []
  [sheet/sheet1 @sheet {:update-cell update-cell
                        :set-mode set-mode
                        :edit-mode #(set-mode % :edit)}])

(defn ^:export main []
  (reset! sheet (grid/eval-sheet (start-sheet)))
  (r/render
   [active-sheet]
   (.getElementById js/document "app")))
