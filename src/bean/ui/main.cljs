(ns bean.ui.main
  (:require [reagent.dom :as r]
            [reagent.core :as rc]
            [bean.grid :as grid]
            [bean.ui.sheet :as sheet]))

(def num-rows 20)
(def num-cols 12)

(defn- start-sheet []
  (vec
   (for [_ (range num-rows)]
     (vec (map (fn [_] "") (range num-cols))))))

(defonce sheet (rc/atom (grid/eval-sheet (start-sheet))))
(def selected-cell (rc/atom nil))

(defn update-cell [address content]
  (swap! sheet #(grid/eval-sheet % address content)))

(defn cell-selector []
  (when @selected-cell
   [:div
    {:id :cell-selector
     :style {:top (str (+ 30 (* (first @selected-cell) 30)) "px")
             :left (str (+ 40 350 (* (second @selected-cell) 110)) "px")
             :display :block}}]))

(defn set-mode [[r c] mode]
  (swap! sheet #(update-in % [:grid r c :mode] (constantly mode)))
  (when (= mode :edit) (reset! selected-cell [r c])))

(defn active-sheet []
  [sheet/sheet1
   num-rows
   num-cols
   @sheet
   {:update-cell update-cell
    :set-mode set-mode
    :edit-mode #(set-mode % :edit)}])

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
      :spell-check false}
     (apply str "let area {x+x}" (for [_ (range num-rows)] "\n"))]]])

(defn ^:dev/after-load ^:export main []
  (r/render
   [:div {:class :container}
    [cell-selector]
    [scratch]
    [active-sheet]]
   (.getElementById js/document "app")))
