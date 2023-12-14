(ns bean.ui.db
  (:require [bean.grid :as grid]))

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
         [:help-display boolean?]]]])

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
