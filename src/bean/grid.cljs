(ns bean.grid
  (:require [bean.interpreter :as interpreter]
            [bean.parser :as parser]
            [bean.util :as util]
            [clojure.set :as set]
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
;       (comment reduce #(depgraph-add-edge %1 (:parent [:binding ...]) (:child %2)) {} bindings)
       (reduce #(depgraph-add-edge %1 (:parent %2) (:child %2)) {})))

(comment defn eval-dep [[deptype dep]]
  (case deptype
    :ref (eval-sheet dep)
    :binding (eval-binding dep)))

(comment
  A1=5, foo=A1+x, B1=foo (4), M1=A1+8
  [:ref A1] [:binding foo]
  [:ref A1] [:ref M1]
  [:binding foo] [:ref B1]
  if deps, eval deps
  )

(comment
  on scratch evaluation trigger

  from old deps, for binding parents, re-evaluate children
  update deps
  )

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

(defn- dependents [addrs depgraph]
  (->> addrs
       (map depgraph)
       (mapcat identity)
       set))

(defn- interested-spillers [addrs grid]
  (->> addrs
       (mapcat #(get-in grid (conj % :interested-spillers)))
       set))

(defn- make-sheet [parsed-grid]
  {:grid parsed-grid
   :bindings {}
   :depgraph (make-depgraph parsed-grid)})

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
         updated-addrs (set/union evaled-addrs cleared-addrs)
         addrs-to-reval (-> (set/union
                             (dependents updated-addrs depgraph)
                             (interested-spillers updated-addrs grid))
                            (disj address))]
     (reduce
      eval-sheet
      {:grid grid*
       :depgraph (cond-> depgraph
                   content-changed? (update-depgraph
                                     address
                                     existing-cell
                                     cell*))
       :ui (or ui {})}
      addrs-to-reval))))

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
