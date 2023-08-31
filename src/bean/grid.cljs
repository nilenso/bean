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

(defn- evaluate-grid-cell [grid address]
  (assoc-in grid
            address
            (interpreter/eval-cell (util/get-cell grid address)
                                   address
                                   grid)))

(defn- offset [[start-row start-col] [offset-rows offset-cols]]
  [(+ start-row offset-rows) (+ start-col offset-cols)])

(defn- set-spilled-cell [grid address cell]
  (assoc-in grid address cell))

(defn- update-cell [grid address f]
  (update-in grid address f))

(defn- disaddress [cell]
  (dissoc cell :address :relative-address))

(defn- set-spill-error [grid address]
  (-> grid
      (assoc-in (conj address :value) nil)
      (assoc-in (conj address :error) "Spill error")
      (assoc-in (conj address :representation) "Spill error")))

(defn- spill-matrices [grid unspilled-cells]
  (letfn [(desired-spillage [{:keys [matrix address] :as cell}]
                            (->> (util/map-on-matrix-addressed
                                  #(cond-> {:relative-address %1
                                            :spilled-from address
                                            :error (:error %2)
                                            :representation (:representation %2)
                                            :value (:value %2)}
                                     (= %1 [0 0]) (merge {:matrix matrix
                                                          :content (:content cell)
                                                          :ast (:ast cell)
                                                          :dependencies (:dependencies cell)}))
                                  matrix)
                                 flatten))
          (apply-spillage [grid spillage]
            (loop [initial-grid grid
                   spilled-grid grid
                   spillage spillage]
              (if (first spillage)
                (let [{:keys [spilled-from relative-address] :as cell} (first spillage)
                      address (offset spilled-from relative-address)
                           ;; TODO: could have error or value
                      value-blank? (not (:value (util/get-cell spilled-grid address)))
                      is-origin? (= [0 0] relative-address)]
                  (if (or value-blank? is-origin?)
                    (recur initial-grid
                           (set-spilled-cell spilled-grid address (disaddress cell))
                           (rest spillage))
                    (set-spill-error initial-grid spilled-from)))
                spilled-grid)))]
    (->> unspilled-cells
         (map desired-spillage)
         (reduce apply-spillage grid))))

(defn- address-matrix [matrix]
  (util/map-on-matrix-addressed #(assoc %2 :address %1) matrix))

(defn- clear-matrix [grid address]
  (->> (:matrix (util/get-cell grid address)) ; error should be handled here
       (util/map-on-matrix-addressed (fn [a _] (offset address a)))
       (mapcat identity)
       (reduce (fn [grid* addr]
                 (update-cell grid*
                              addr
                              #(-> %
                                   (dissoc :value :error)
                                   (assoc :representation ""))))
               grid)))

(defn evaluate-grid
  ([grid]
   (let [parsed-grid (util/map-on-matrix content->cell grid)
         unspilled-grid (util/map-on-matrix-addressed #(interpreter/eval-cell %2 %1 parsed-grid)
                                                      parsed-grid)
         evaluated-grid (->> unspilled-grid
                             address-matrix
                             flatten
                             (filter :matrix)
                             (spill-matrices unspilled-grid))
         depgraph (depgraph evaluated-grid)]
     {:grid evaluated-grid
      :depgraph depgraph}))

  ([address grid depgraph]
   (let [dependents (get depgraph address #{})
         unspilled-grid (evaluate-grid-cell grid address)
         unspilled-cell (util/get-cell unspilled-grid address)
         evaluated-grid (cond-> unspilled-grid
                          (:matrix unspilled-cell)
                          (spill-matrices [(assoc unspilled-cell :address address)]))
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
   (let [old-unspilled-cell (util/get-cell grid address)
         dependents (cond-> (get depgraph address #{})
                      (:spilled-from old-unspilled-cell) (conj (:spilled-from old-unspilled-cell)))
         updated-grid (cond-> (update-cell grid address
                                           #(merge % {:content new-content
                                                      :ast (parser/parse new-content)}))
                        ;; clear intersecting matrices to prepare for a redraw
                        (:spilled-from old-unspilled-cell) (clear-matrix (:spilled-from old-unspilled-cell)))
         unspilled-grid (evaluate-grid-cell updated-grid address)
         unspilled-cell (util/get-cell unspilled-grid address)
         evaluated-grid (cond-> unspilled-grid
                          (:matrix unspilled-cell) (spill-matrices [(assoc unspilled-cell :address address)]))
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
        :depgraph updated-depgraph}))))
(comment
  {
   ;; User input
   :content nil

   ;; Internal representation of user input
   :ast nil

   ;; Evaluation results
   :value nil
   :representation nil
   :error nil
   :matrix nil

   ;; Evaluation metadata
   :spilled-from nil
   :dependencies nil

   ;; Addressing information
   :address nil
   :relative-address nil})

