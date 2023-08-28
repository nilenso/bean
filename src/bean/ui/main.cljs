(ns bean.ui.main
  (:require [reagent.dom :as r]
            [reagent.core :as rc]
            [bean.ui.sheet :as sheet]))

(def sheet (rc/atom nil))

(defn cell [val]
  {:value val
   :error (> (rand) 0.9)
   :representation (str val)
   :ast [:CellContents [:Constant [:String (str val)]]]
   :content (str val)})

(defn- start-sheet []
  (for [row-num (range 50)]
    (vec (map #(cell (str "Cell " %)) (range 26)))))

(defn ^:export main []
  (reset! sheet (start-sheet))
  (r/render [sheet/sheet1 @sheet]
            (.getElementById js/document "app")))
