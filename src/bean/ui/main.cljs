(ns bean.ui.main
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.sheet :as sheet]
            [bean.ui.scratch :as scratch]
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
          :ui {:row-heights (vec (repeat num-rows 30))
               :col-widths (vec (repeat num-cols 110))
               :selected-cell nil})))

(defonce ui-state
  (rc/atom {:help-display :none}))

(defn update-cell [address content]
  (swap! sheet1 #(grid/eval-address [:cell address] % content)))

(defn resize-row [row height]
  (swap! sheet1 #(assoc-in % [:ui :row-heights row] height)))

(defn resize-col [col width]
  (swap! sheet1 #(assoc-in % [:ui :col-widths col] width)))

(defn set-mode [[r c] mode]
  (swap! sheet1 #(update-in % [:grid r c :mode] (constantly mode)))
  (when (= mode :edit) (swap! sheet1 #(assoc-in % [:ui :selected-cell] [r c]))))

(defn explain [expression]
  (println (provenance/sentence-proof expression @sheet1)))

(defn active-sheet []
  [sheet/sheet
   @sheet1
   {:update-cell update-cell
    :set-mode set-mode
    :edit-mode #(set-mode % :edit)
    :resize-col resize-col
    :resize-row resize-row}])

(defn help []
  [:div {:id :help-container
         :class :help-container
         :style {:display (:help-display @ui-state)}
         :on-click #(swap! ui-state (fn [s] (assoc s :help-display "none")))}
   [:div {:class :help
          :on-click #(.stopPropagation %)}
    ""
    [:button {:class [:small-btn :close-help]
              :on-click #(swap! ui-state (fn [s] (assoc s :help-display "none")))} "×"]]])

(defn container []
  [:div {:class [:container
                 (when (= (:help-display @ui-state) "block")
                   "help-open")]}
   [help]
   [:div {:class :sheet-container}
    [scratch/text-area sheet1 ui-state]
    [active-sheet]]])

(defn ^:dev/after-load ^:export main []
  (r/render
   [container]
   (.getElementById js/document "app")))
