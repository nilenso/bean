(ns bean.grid
  (:require [bean.interpreter :as interpreter]
            [bean.parser :as parser]
            [bean.util :as util]
            [bean.value :as value]
            [bean.deps :as deps]
            [bean.errors :as errors]
            [clojure.set :as set]
            [clojure.string]))

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
  (util/map-on-matrix value/from-cell grid))

(defn- eval-cell [cell sheet]
  (if (or (not (:spilled-from cell))
          (:matrix cell))
    (interpreter/eval-cell cell sheet)
    cell))

(defn- dependents [addrs depgraph]
  (->> addrs
       (map depgraph)
       (mapcat identity)
       set))

(defn- interested-spillers [addrs grid]
  (->> addrs
       (mapcat #(get-in grid (conj % :interested-spillers)))
       (map deps/->cell-dep)
       set))

(defn new-sheet [content-grid code]
  (let [parsed-grid (parse-grid content-grid)]
    {:grid parsed-grid
     :code code
     :bindings {}
     :depgraph (deps/make-depgraph parsed-grid)}))

(defn- escalate-bindings-errors [sheet]
  (reduce (fn [sheet [named {:keys [error] :as v}]]
            (if error
              (reduced (assoc sheet :code-error (errors/named-ref-error named error)))
              sheet))
          (dissoc sheet :code-error)
          (:bindings sheet)))

(defmulti eval-address first)
;; not a fan of making this a defmulti
;; when we make this iterative instead of recursive, we'll have to undo this

(defmethod eval-address :cell
  ([[_ cell-address :as address] {:keys [grid] :as sheet}]
   (eval-address address sheet (util/get-cell grid cell-address) false))

  ([address sheet new-content]
   (eval-address address sheet (value/from-cell new-content) true))

  ([[_ cell-address :as address] {:keys [grid depgraph ui] :as sheet} cell content-changed?]
   ; todo: if cyclic dependency break with error
   (let [existing-cell (util/get-cell grid cell-address)
         cell* (eval-cell cell sheet)
         unspilled-grid (-> grid
                            (clear-matrix cell-address existing-cell)
                            (assoc-in cell-address cell*))
         cleared-addrs (:spilled-into existing-cell)
         [grid* evaled-addrs] (if (:matrix cell*)
                                (spill-matrix unspilled-grid cell-address)
                                [unspilled-grid #{cell-address}])
         updated-addrs (set/union evaled-addrs cleared-addrs)]

     (as-> (-> sheet
               (assoc :grid grid*)
               (assoc :depgraph (cond-> depgraph
                                  content-changed? (deps/update-depgraph
                                                    address
                                                    existing-cell
                                                    cell*))))
           sheet
       (reduce #(eval-address %2 %1) sheet
               (-> (dependents (map deps/->cell-dep updated-addrs) depgraph)
                   (disj address)))
       ;; The interested spillers here are re-evaluated
       ;; to mark them as spill errors
       (reduce #(eval-address %2 %1) sheet
               (-> (interested-spillers updated-addrs grid)
                   (disj address)))))))

(defmethod eval-address :named
  ([[_ named :as address] {:keys [bindings] :as sheet}]
   (if-let [v (bindings named)]
     (eval-address address sheet v false)
     (errors/undefined-named-ref named)))

  ([address sheet new-content]
  ;; TODO: This is invalid
   (eval-address address
                 sheet
                 (value/from-statement (parser/statement-source (:code sheet)
                                                                new-content)
                                       new-content)
                 true))

  ([[_ named :as address] {:keys [bindings] :as sheet} val _content-changed?]
   (-> (let [existing-val (bindings named)
             val* (-> val
                      (dissoc :error)
                      (merge (interpreter/eval-ast (:ast val) sheet)))]
         (as-> sheet sheet
           (assoc-in sheet [:bindings named] val*)
           (update-in sheet [:depgraph] #(deps/update-depgraph % address existing-val val*))
           (reduce #(eval-address %2 %1)
                   sheet
                   (-> (dependents [address] (:depgraph sheet))
                       (disj address)))))
       escalate-bindings-errors)))

(defn- eval-grid [sheet]
  (util/reduce-on-sheet-addressed
   (fn [sheet address _]
     (eval-address (deps/->cell-dep address) sheet))
   sheet))

(defn eval-code
  ;; Suppressing errors so we let the grid evaluate before showing any errors in the code
  ([sheet] (eval-code sheet (:code sheet) true))
  ([sheet code & suppress-errors]
   (let [res (let [code-ast (parser/parse-statement code)]
               (if-let [parse-error (parser/error code-ast)]
                 (assoc sheet :code-error parse-error)
                 (-> (reduce (fn [sheet [_ [_ named] expr]]
                               (eval-address [:named named]
                                             sheet
                                             (value/from-statement (parser/statement-source code expr)
                                                                   expr)
                                             false))
                             (dissoc sheet :code-error)
                             (rest code-ast))
                     (assoc :code-ast code-ast))))]
     (if (true? suppress-errors)
       res
       (escalate-bindings-errors res)))))



(defn eval-sheet
  ([sheet]
   (->> sheet
        eval-code
        eval-grid
        escalate-bindings-errors)))

(comment
  :cell
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
  

(comment
  :sheet
  {;; Source fields
   :grid grid
   :code code

   ;; Evaluated fields
   :depgraph depgraph
   :bindings bindings

   ;; UI fields
   :grid-dimensions grid-dimensions})
