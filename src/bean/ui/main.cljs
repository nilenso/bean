(ns bean.ui.main
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.sheet :as sheet]
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

(defn scratch []
  [:div {:class :scratch}
   [:div {:class "scratch-header bean-label"}]
   [:div {:class :scratch-thick-lines}]
   [:div {:class :scratch-body}
    [:div {:class :scratch-margin}]
    [:textarea
      ;; TODO: The textarea and the scratch should keep expanding as more text is added
     {:class :scratch-text
      :content-editable ""
      :spell-check false
      :default-value ""}]]])

(defn ^:dev/after-load ^:export main []
  (r/render
   [:div {:class :container}
    [scratch]
    [active-sheet]]
   (.getElementById js/document "app")))
