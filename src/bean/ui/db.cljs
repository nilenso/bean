(ns bean.ui.db
  (:require [bean.grid :as grid]
            [bean.ui.styles :as styles]))

(defn- start-sheet [num-rows num-cols]
  (grid/new-sheet
   (vec
    (for [_ (range num-rows)]
      (vec (map (fn [_] "") (range num-cols)))))
"add:{x+y}
inc:{x+1}
sum:{x.reduce({x + y})}
count:{x.reduce(inc 0)}
concatt:{x.concat(y)}"))

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
                               [:num-cols pos-int?]
                               [:row-heights [:vector pos-int?]]
                               [:col-widths [:vector pos-int?]]]]
            [:grid [:vector [:vector any?]]]
            [:code string?]

            [:depgraph any?]
            [:bindings [:map]]
            [:code-in-editor {:optional true} [:maybe string?]]
            [:code-error {:optional true} [:maybe string?]]
            [:code-ast {:optional true} [:maybe vector?]]]]
   [:ui [:map
         [:help-display boolean?]
         [:grid [:map
                 [:editing-cell {:optional true}]
                 [:selection [:vector [:map
                                       [:start nat-int?]
                                       [:end nat-int?]]]]
                 [:selection-start [:vector nat-int?]]]]]]])

(defn initial-app-db []
  (let [num-rows (:num-rows styles/sizes)
        num-cols (:num-cols styles/sizes)]
    {:sheet (-> (grid/eval-sheet (start-sheet num-rows num-cols))
                (assoc :grid-dimensions {:num-rows num-rows
                                         :num-cols num-cols
                                         :row-heights (vec (repeat num-rows (:cell-h styles/sizes)))
                                         :col-widths (vec (repeat num-cols (:cell-w styles/sizes)))}))
     :ui {:help-display false
          :grid {:editing-cell nil
                 :selection nil
                 :selected-frame nil}}}))
