(ns bean.grid
  (:require [bean.interpreter :as interpreter]
            [bean.parser :as parser]
            [bean.util :as util]
            [bean.deps :as deps]
            [clojure.set :as set]
            [clojure.string]))

(defn- content->cell
  ([content]
   {:content content
    :ast (parser/parse content)}))

(defn- offset [[start-row start-col] [offset-rows offset-cols]]
  [(+ start-row offset-rows) (+ start-col offset-cols)])

(defn- set-spilled-cell [grid address cell]
  (assoc-in grid address cell))

(defn- set-spill-error [grid address]
  (-> grid
      (assoc-in (conj address :value) nil)
      (assoc-in (conj address :error) "Spill error")
      (assoc-in (conj address :representation) "Spill error")))

(defn- clear-spilled-cell [cell address]
  (-> cell
      (dissoc :value :error :spilled-from)
      (update :interested-spillers disj address)
      (assoc :representation "")))

(defn- clear-matrix
  [grid address {:keys [spilled-into]}]
  (->> spilled-into
       (reduce
        (fn [grid* addr]
          (if (and
               (or (empty? (get-in grid (conj addr :content)))
                   (= addr address))
               (= (get-in grid (conj addr :spilled-from)) address))
            (update-in grid* addr #(clear-spilled-cell % address))
            grid*))
        grid)))

(defn- spill-matrix [grid address]
  (letfn
   [(desired-spillage
     [{:keys [matrix] :as cell}]
     "Returns a collection of cells that the given cell intends to spill"
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

    (express-interests
      [grid spillage]
      "Marks cells in a given grid for potential spillage. The potential spillage
       information is stored in the cell structure's `:interested-spillers` field.

       The :interested-spillers field is used to track spillers that need to be
       re-evaluated when a conflicting spiller is removed. eg. When two cells
       spill into a common cell and one of the cells stops spilling into the
       common cell, the other spiller must not have a spill error anymore and
       spill into the (previously common) cell successfully"
      (reduce
       #(let [{:keys [spilled-from relative-address]} %2
              address (offset spilled-from relative-address)
              cell (util/get-cell %1 address)
              existing-spillers (get cell :interested-spillers #{})]
          (assoc-in %1
                    (conj address :interested-spillers)
                    (conj existing-spillers spilled-from)))
       grid
       spillage))

    (spill
      [grid spillage]
      "Update the grid with the spillage 'applied' into the grid. If the spillage
       conflicts with existing content (or spillage), the spiller is marked as a
       spill error and none of the spillage is applied."
      (let [grid1 (express-interests grid spillage)]
        (loop [initial-grid grid1
               spilled-grid grid1
               updated-addresses #{}
               spillage spillage]
          (if (first spillage)
            (let [{:keys [spilled-from relative-address] :as spilled-cell} (first spillage)
                  address* (offset spilled-from relative-address)
                  cell (util/get-cell spilled-grid address*)
                  blank? (empty? (:content cell))
                  spilled? (:spilled-from cell)
                  is-spiller? (= relative-address [0 0])]
              (if (or is-spiller? (and (not spilled?) blank?))
                (recur initial-grid
                       (set-spilled-cell
                        spilled-grid
                        address*
                        (-> spilled-cell
                            (assoc :interested-spillers (:interested-spillers cell))))
                       (conj updated-addresses address*)
                       (rest spillage))
                [(set-spill-error initial-grid spilled-from) #{address}]))
            [(assoc-in spilled-grid (conj address :spilled-into) updated-addresses)
             updated-addresses]))))]
    (->> (desired-spillage (util/get-cell grid address))
         (spill grid))))

(defn parse-grid [grid]
  (util/map-on-matrix content->cell grid))

(defn- eval-cell [cell sheet]
  (if (or (not (:spilled-from cell))
          (:matrix cell))
    (interpreter/eval-cell cell sheet)
    cell))

(defn- interested-spillers [addrs grid]
  (->> addrs
       (mapcat #(get-in grid (conj % :interested-spillers)))
       set))

(defn- make-sheet [parsed-grid]
  {:grid parsed-grid
   :bindings {}
   :depgraph (deps/make-depgraph parsed-grid)})

(defn eval-sheet
  ([content-grid]
   (let [sheet (make-sheet (parse-grid content-grid))]
     (util/reduce-on-sheet-addressed
      (fn [sheet address _]
        (eval-sheet sheet address))
      sheet)))

  ([{:keys [grid] :as sheet} address]
   (eval-sheet sheet address (util/get-cell grid address) false))

  ([sheet address new-content]
   (eval-sheet sheet address (content->cell new-content) true))

  ([{:keys [grid depgraph ui] :as sheet} address cell content-changed?]
   ; todo: if cyclic dependency break with error
   (let [existing-cell (util/get-cell grid address)
         cell* (eval-cell cell sheet)
         unspilled-grid (-> grid
                            (clear-matrix address existing-cell)
                            (assoc-in address cell*))
         cleared-addrs (:spilled-into existing-cell)
         [grid* evaled-addrs] (if (:matrix cell*)
                                (spill-matrix unspilled-grid address)
                                [unspilled-grid #{address}])
         updated-addrs (set/union evaled-addrs cleared-addrs)]
     (as-> {:grid grid*
            :depgraph (cond-> depgraph
                        content-changed? (deps/update-depgraph
                                          address
                                          existing-cell
                                          cell*))
            :ui (or ui {})} sheet
       (reduce eval-sheet
               sheet
               (-> (interested-spillers updated-addrs grid)
                   (disj address)))
       (reduce eval-sheet
               sheet
               (->> updated-addrs
                    (deps/immediate-dependents sheet)
                    (deps/resolve-dependents sheet)))))))

;; 
;; ~~add binding dependencies to graph~~
;; hardcoded scratch with bindings
;; cells depend on binding
;; eval scratch is run on page load which evaluates the hardcoded function declaration in scratch
;; 

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
   :spilled-into nil
   :interested-spillers #{}

   ;; Addressing information
   :relative-address nil})
