(ns bean.ui.main
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.sheet :as sheet]
            [bean.ui.code :as code]
            [bean.ui.help :as help]
            [re-frame.core :as rf]
            [malli.core :as m]
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

(def Cell
  [:map
   [:content string?]
   [:ast [:vector]]
   [:scalar any?]
   [:matrix vector?]
   [:spilled-from [:maybe string?]]
   [:representation string?]])

(def AppDb
  [:map
   [:sheet [:map
            [:grid-dimensions [:map
                               [:num-rows pos-int?]
                               [:num-cols pos-int?]]]
            [:ui [:map
                  [:row-heights [:vector pos-int?]]
                  [:col-widths [:vector pos-int?]]
                  [:selected-cell
                   [:maybe
                    [:tuple pos-int? pos-int?]]]]]

            [:grid [:vector [:vector any?]]]
            [:code string?]

            [:depgraph any?]
            [:bindings [:map]]
            [:code-in-editor {:optional true} [:maybe string?]]
            [:code-error {:optional true} [:maybe string?]]
            [:code-ast {:optional true} [:maybe vector?]]]]
   ;; TODO: Maybe the [:sheet :ui] path key needs to be renamed
   [:ui [:map
         [:help-display :none]]]])

(defn initial-app-db []
  (let [num-rows 20
        num-cols 12]
    {:sheet (-> (grid/eval-sheet (start-sheet))
                (assoc :grid-dimensions {:num-rows num-rows
                                         :num-cols num-cols})
                (assoc :ui {:row-heights (vec (repeat num-rows 30))
                            :col-widths (vec (repeat num-cols 110))
                            :selected-cell nil}))
     :ui {:help-display :none}}))

(defonce sheet1
  (rc/atom (:sheet (initial-app-db))))

(defonce ui-state
  (rc/atom (:ui (initial-app-db))))

(defn update-cell [address content]
  (swap! sheet1 #(grid/eval-cell address % content)))

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

(defn container []
  [:div {:class [:container
                 (when (= (:help-display @ui-state) "block")
                   "help-open")]}
   [help/help ui-state]
   [:div {:class :sheet-container}
    [code/text-area sheet1 ui-state]
    [active-sheet]]])

(defn ^:dev/after-load ^:export main []
  (r/render
   [container]
   (.getElementById js/document "app")))
