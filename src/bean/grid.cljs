(ns bean.grid
  (:require [bean.parser :as parser]
            [bean.interpreter :as interpreter]
            [bean.util :as util]))

(defn- content->cell
  ([content]
   {:content content
    :ast (parser/parse content)}))

(defn- depgraph-add-edge [depgraph parent child]
  (assoc depgraph parent (conj (get depgraph parent #{}) child)))

(defn- depgraph-remove-edge [depgraph parent child]
  (assoc depgraph parent (disj (get depgraph parent) child)))

(defn depgraph [grid]
  (let [edges (->> grid
                   (util/map-on-matrix-addressed
                    #(for [dependency (:dependencies %2)]
                       {:parent dependency :child %1}))
                   flatten)]
    (reduce
     #(depgraph-add-edge %1 (:parent %2) (:child %2))
     {}
     edges)))

(defn- update-depgraph [depgraph address old-dependencies new-dependencies]
  (as-> depgraph g
    (reduce #(depgraph-remove-edge %1 %2 address) g old-dependencies)
    (reduce #(depgraph-add-edge %1 %2 address) g new-dependencies)))

(defn- evaluate-partial-grid [grid address]
  (assoc-in grid
            address
            (interpreter/eval-cell (util/get-cell grid address)
                                   address
                                   grid)))

(defn- spillage [cell address]
  {:origin-address [0 0]
   :spillage [{:address [0 1]
               :value 5}]})

(defn- spill-matrices [unspilled-grid]
  (comment
    collect spilled matrix dimensions
    check for spillage intersections
    Mark origin cells as error for intersecting spills
    Update values for non intersecting spillage)
  (->> unspilled-grid
       (util/map-on-matrix-addressed #(do [%2  %1]))
       (concat)
       (filter #(get-in % [0 :matrix]))
       (map spillage)
       (reduce (fn [grid1 {:keys [origin-address spillage]}]
                 (loop [grid2 grid1
                        spillage spillage]
                   (if-let [{:keys [address value]} (first spillage)]
                     (if (empty-cell? (get-cell grid2 address))
                       (recur (set-spilled-value grid2 address value)
                              (rest spillage))
                       grid1) ;; TODO: Should set the spill error on the origin cell
                     grid2))
                 spillage)
               unspilled-grid))

  (map (fn [[address origin-cell]]
         (spillage origin-cell address))
       (->> unspilled-grid
            (util/map-on-matrix-addressed #(do [%1  %2]))
            (concat)
            (filter #(get-in % [1 :matrix])))))

(defn evaluate-grid
  ([grid]
   (let [parsed-grid (util/map-on-matrix content->cell grid)
         evaluated-grid (->> parsed-grid
                             (util/map-on-matrix-addressed #(interpreter/eval-cell %2 %1 parsed-grid))
                             spill-matrices)
         depgraph (depgraph evaluated-grid)]
     {:grid evaluated-grid
      :depgraph depgraph}))

  ([address grid depgraph]
   (let [dependents (get depgraph address #{})
         evaluated-grid (evaluate-partial-grid grid address)
         old-dependencies (:dependencies (util/get-cell grid address))
         new-dependencies (:dependencies (util/get-cell evaluated-grid address))
         updated-depgraph (update-depgraph depgraph address old-dependencies new-dependencies)]
     ;; Need to re-look at how the depgraph recomposition works.
     (if dependents
       (reduce #(evaluate-grid %2 (:grid %1) (:depgraph %1))
               {:grid evaluated-grid
                :depgraph updated-depgraph}
               dependents)
       {:grid evaluated-grid
        :depgraph updated-depgraph})))

  ([address new-content grid depgraph]
   (evaluate-grid address
                  (assoc-in grid address (content->cell new-content))
                  depgraph)))
