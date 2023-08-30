(ns bean.grid
  (:require [bean.interpreter :as interpreter]
            [bean.parser :as parser]
            [bean.util :as util]
            [clojure.string]))

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

(defn- offset [[start-row start-col] [offset-rows offset-cols]]
  [(+ start-row offset-rows) (+ start-col offset-cols)])

(defn- spillage [{:keys [matrix content]} address]
  {:origin-address address
   :spillage (->> (util/map-on-matrix-addressed
                   #(do
                      {:relative-address %1
                       :cell (merge
                              {:error (:error %2)
                               :representation (:representation %2)
                               :value (:value %2)}
                              (when (= %1 [0 0])
                                {:matrix matrix
                                 :content content}))})
                   matrix)
                  flatten)})

(defn- set-spilled-cell [grid address cell]
  (assoc-in grid address cell))

(defn- set-spill-error [grid address]
  (-> grid
      (assoc-in (conj address :value) nil)
      (assoc-in (conj address :error) "Spill error")
      (assoc-in (conj address :representation) "Spill error")))

(defn- spill-matrices [unspilled-grid]
  (let [spillages (->> unspilled-grid
                       (util/map-on-matrix-addressed #(do [%2 %1]))
                       (mapcat identity)
                       (filter #(get-in % [0 :matrix]))
                       (map #(apply spillage %1))
                       flatten)]
    (reduce
     (fn [grid1 {:keys [origin-address spillage]}]
       (loop [grid2 grid1
              spillage spillage]
         (if (first spillage)
           (let [{:keys [relative-address cell]} (first spillage)
                 address (offset origin-address relative-address)
                 value-blank? (not (:value (util/get-cell grid2 address)))
                 is-origin? (= [0 0] relative-address)]
             (if (or value-blank? is-origin?)
               (recur (set-spilled-cell grid2 address cell)
                      (rest spillage))
               (set-spill-error grid1 origin-address)))
           grid2)))
     unspilled-grid
     spillages)))

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
