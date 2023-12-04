(ns bean.ui.main
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.sheet :as sheet]
            [bean.ui.scratch :as scratch]
            [bean.ui.help :as help]
            [reagent.core :as rc]
            [reagent.dom :as r]))

(def num-rows 20)
(def num-cols 12)

(defn- start-sheet []
  (grid/new-sheet
   (vec
    (for [_ (range num-rows)]
      (vec (map (fn [_] "") (range num-cols)))))
   ""))

(defonce sheet1
  (rc/atom
   (assoc (grid/eval-sheet (start-sheet))
          :presentation {:row-heights (vec (repeat num-rows 30))
                         :col-widths (vec (repeat num-cols 110))})))

(defonce ui-state
  (rc/atom {:help-display :none
            :selections []}))

(defn update-cell [address content]
  (swap! sheet1 #(grid/eval-cell address % content)))

(defn resize-row [row height]
  (swap! sheet1 #(assoc-in % [:presentation :row-heights row] height)))

(defn resize-col [col width]
  (swap! sheet1 #(assoc-in % [:presentation :col-widths col] width)))

(defn set-mode [[r c] mode]
  (swap! sheet1 #(update-in % [:grid r c :mode] (constantly mode)))
  (when (= mode :edit)
    (swap! ui-state #(assoc % :selections [{:start [r c] :end [r c]}]))))

(defn explain [expression]
  (println (provenance/sentence-proof expression @sheet1)))

(defn active-sheet []
  [sheet/sheet
   @sheet1
   @ui-state
   {:update-cell update-cell
    :set-mode set-mode
    :edit-mode #(set-mode % :edit)
    :resize-col resize-col
    :resize-row resize-row}])

(defn container []
  [:div {:class [:container
                 (when (= (:help-display @ui-state) "block")
                   "help-open")]}
   [help/help ui-state]
   [:div {:class :sheet-container}
    [scratch/text-area sheet1 ui-state]
    [active-sheet]]])

(defn ^:dev/after-load ^:export main []
  (r/render
   [container]
   (.getElementById js/document "app")))
