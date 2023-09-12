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

(defn make-depgraph [grid]
  (->> grid
       (util/map-on-matrix-addressed
        #(for [dependency (interpreter/ast->deps (:ast %2))]
           {:parent dependency :child %1}))
       flatten
       (reduce #(depgraph-add-edge %1 (:parent %2) (:child %2)) {})))

(defn- update-depgraph [depgraph address old-cell new-cell]
  (let [old-dependencies (interpreter/ast->deps (:ast old-cell))
        new-dependencies (interpreter/ast->deps (:ast new-cell))]
    (as-> depgraph g
      (reduce #(depgraph-remove-edge %1 %2 address) g old-dependencies)
      (reduce #(depgraph-add-edge %1 %2 address) g new-dependencies))))

(defn- offset [[start-row start-col] [offset-rows offset-cols]]
  [(+ start-row offset-rows) (+ start-col offset-cols)])

(defn- set-spilled-cell [grid address cell]
  (assoc-in grid address cell))

(defn- disaddress [cell]
  (dissoc cell :address :relative-address))

(defn- set-spill-error [grid address]
  (-> grid
      (assoc-in (conj address :value) nil)
      (assoc-in (conj address :error) "Spill error")
      (assoc-in (conj address :representation) "Spill error")))

(defn- clear-spilled-cell [cell]
  (-> cell
      (dissoc :value :error)
      (assoc :representation "")))

(defn- spill-matrix [grid address]
  (letfn
   [(clearable-addresses
      [address matrix]
      (->> matrix
           (util/map-on-matrix-addressed (fn [a _] (offset address a)))
           (mapcat identity)))

    (clear-matrix
      [grid address {:keys [matrix]}]
      (->> matrix
           (clearable-addresses address)
           (reduce
            (fn [grid* addr]
              (if (and
                   (or (empty? (get-in grid (conj addr :content)))
                       (= addr address))
                   (= (get-in grid (conj addr :spilled-from)) address))
                (update-in grid* addr clear-spilled-cell)
                grid*))
            grid)))

    (desired-spillage
      [{:keys [matrix] :as cell}]
      (->> matrix
           (util/map-on-matrix-addressed
            #(cond-> {:relative-address %1
                      :spilled-from address
                      :error (:error %2)
                      :representation (:representation %2)
                      :value (:value %2)}
               (= %1 [0 0]) (merge {:matrix matrix
                                    :content (:content cell)
                                    :ast (:ast cell)})))
           flatten))

    (apply-spillage
      [grid spillage]
      (loop [initial-grid grid
             spilled-grid grid
             updated-addresses #{}
             spillage spillage]
        (if (first spillage)
          (let [{:keys [spilled-from relative-address] :as spilled-cell} (first spillage)
                address (offset spilled-from relative-address)
                cell (util/get-cell spilled-grid address)
                blank? (and (empty? (:content cell))
                            (not (or (:value cell) (:error cell))))
                already-spilled-from (:spilled-from cell)
                spillable? (or (= already-spilled-from spilled-from)
                               (nil? already-spilled-from))
                is-origin? (= [0 0] relative-address)]
            (if (and spillable? (or blank? is-origin?))
              (recur initial-grid
                     (set-spilled-cell spilled-grid address (disaddress spilled-cell))
                     (conj updated-addresses address)
                     (rest spillage))
              [(set-spill-error initial-grid spilled-from) #{}]))
          [spilled-grid updated-addresses])))]
    (let [unspilled-cell (util/get-cell grid address)]
      (->> unspilled-cell
           desired-spillage
           (apply-spillage (clear-matrix grid address unspilled-cell))))))

(defn parse-grid [grid]
  (util/map-on-matrix content->cell grid))

(defn eval-sheet
  ([content-grid]
   (let [parsed-grid (parse-grid content-grid)
         sheet {:grid parsed-grid
                :depgraph (make-depgraph parsed-grid)}]
     (util/reduce-on-sheet-addressed
      (fn [sheet address _]
        (eval-sheet sheet address))
      sheet)))

  ([{:keys [grid] :as sheet} address]
   (eval-sheet sheet address (util/get-cell grid address) false))

  ([sheet address new-content]
   (eval-sheet sheet address (content->cell new-content) true))

  ([{:keys [grid depgraph]} address cell update-depgraph?]
   ; todo: if cyclic dependency break with error
   (let [existing-cell (util/get-cell grid address)
         was-spilled-from (:spilled-from existing-cell)
         was-spilled-into? (and was-spilled-from (not= was-spilled-from address))
         cell* (interpreter/eval-cell cell grid)
         unspilled-grid (assoc-in grid address cell*)
         [grid* evaled-addresses] (if (:matrix cell*)
                                    (spill-matrix unspilled-grid address)
                                    [unspilled-grid #{address}])
         dependents (->> evaled-addresses
                         (map depgraph)
                         (mapcat identity))
         cells-to-reval (cond-> dependents
                          was-spilled-into? (conj was-spilled-from))]
     (reduce
      eval-sheet
      {:grid grid*
       :depgraph (cond-> depgraph
                   update-depgraph? (update-depgraph
                                     address
                                     existing-cell
                                     cell*))}
      cells-to-reval))))

(comment
  {;; User input
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

   ;; Addressing information
   :address nil
   :relative-address nil})
