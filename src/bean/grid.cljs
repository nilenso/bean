(ns bean.grid
  (:require [bean.interpreter :as interpreter]
            [bean.parser.parser :as parser]
            [bean.util :as util]
            [bean.value :as value]
            [bean.deps :as deps]
            [bean.errors :as errors]
            [bean.code-errors :as code-errors]
            [clojure.set :as set]
            [clojure.string]
            [bean.parser.trellis-parser :as trellis-parser]))

(defn- offset [[start-row start-col] [offset-rows offset-cols]]
  [(+ start-row offset-rows) (+ start-col offset-cols)])

(defn- set-error [grid address error]
  (update-in grid
             address
             #(errors/mark % error)))

(defn- clear-spilled-cell [cell spiller]
  (-> cell
      errors/reset
      (dissoc :scalar :spilled-from)
      (update :interested-spillers disj spiller)
      (assoc :representation "")))

(defn- empty-spilled-cell? [cell]
  (and (:spilled-from cell)
       (not (:matrix cell))
       (empty? (:content cell))))

(defn- clear-spill
  [grid spiller]
  (reduce
   (fn [grid* addr]
     (if (and (empty? (get-in grid (conj addr :content)))
              (= (get-in grid (conj addr :spilled-from)) spiller))
       (update-in grid* addr #(clear-spilled-cell % spiller))
       grid*))
   grid
   (:spilled-into (util/get-cell grid spiller))))

(defn- spill-matrix [grid spiller]
  (letfn
   [(desired-spillage
      [{:keys [matrix] :as cell}]
      "Returns a collection of cells that the given cell intends to spill"
      (->> matrix
           (util/map-on-matrix-addressed
            #(merge
              {:relative-address %1
               :spilled-from spiller
               :error (:error %2)
               :representation (:representation %2)
               :scalar (:scalar %2)}
              (when (= %1 [0 0])
                {:matrix matrix
                 :content (:content cell)
                 :ast (:ast cell)})))
           flatten))

    (express-interests
      [grid* spillage]
      "Marks cells in a given grid for potential spillage. The potential spillage
      information is stored in the cell structure's `:interested-spillers` field.

      The :interested-spillers field is used to track spillers that need to be
      re-evaluated when a conflicting spiller is removed. eg. When two cells
      spill into a common cell and one of the cells stops spilling into the
      common cell, the other spiller must not have a spill error anymore and
      spill into the (previously common) cell successfully"
      (reduce
       #(let [{:keys [spilled-from relative-address]} %2
              [r c] (offset spilled-from relative-address)
              existing-spillers (get-in %1 [r c :interested-spillers] #{})
              spillers* (conj existing-spillers spilled-from)]
          (assoc-in %1 [r c :interested-spillers] spillers*))
       grid*
       spillage))

    (spill*
      [grid* spillage]
      "Update the grid with the spillage 'applied' into the grid. Returns false
       if the spillage conflicts with existing content (or spillage)."
      (reduce
       #(let [{:keys [relative-address]} %2
              address* (offset spiller relative-address)
              cell (util/get-cell %1 address*)
              blank? (empty? (:content cell))
              spilled-by-other? (:spilled-from cell)
              is-spiller? (= relative-address [0 0])
              spilled-cell (merge %2 {:interested-spillers (:interested-spillers cell)
                                      :style (:style cell)})]
          (if (or is-spiller? (and (not spilled-by-other?) blank?))
            (assoc-in %1 address* spilled-cell)
            (reduced false)))
       grid*
       spillage))

    (spilled-addrs
      [spillage]
      (->> spillage (map #(offset spiller (:relative-address %))) set))

    (spill
      [grid spillage]
      "If the spillage was successfully applied, sets the spiller's spilled-into.
       Otherwise it's marked as spill error."
      (if-let [grid* (spill* grid spillage)]
        (let [updated-addrs (spilled-addrs spillage)]
          [(assoc-in grid* (conj spiller :spilled-into) updated-addrs) updated-addrs])
        [(set-error grid spiller (errors/spill-error)) #{spiller}]))]

    (let [spillage (desired-spillage (util/get-cell grid spiller))]
      (-> grid
          (express-interests spillage)
          (spill spillage)))))

(defn parse-grid [grid]
  (util/map-on-matrix value/from-cell grid))

(defn- eval-cell* [cell sheet]
  (if-not (empty-spilled-cell? cell)
    (let [parsed-cell (assoc cell :ast (parser/parse (:content cell)))]
      (interpreter/eval-cell parsed-cell sheet))
    cell))

(defn- dependents [addrs depgraph]
  (->> addrs
       (mapcat depgraph)
       set))

(defn- interested-spillers [addrs grid]
  (->> addrs
       (mapcat #(get-in grid (conj % :interested-spillers)))
       set))

(defn new-sheet
  ([content-grid code]
   (new-sheet content-grid code ""))
  ([content-grid code tests]
   (let [parsed-grid (parse-grid content-grid)]
     {:grid parsed-grid
      :code code
      :tests tests
      :bindings {}
      :depgraph (deps/make-depgraph parsed-grid)})))

(defn- escalate-bindings-errors [sheet]
  (if (code-errors/get-error sheet)
    ;; This likely already set a parse error, return as is
    sheet
    (reduce (fn [sheet [named {:keys [error]}]]
              (if error
                (reduced (assoc sheet :code-error (code-errors/named-ref-error named error)))
                sheet))
            (dissoc sheet :code-error) ;; reset the error from a previous evaluation
            (:bindings sheet))))

(defn- set-cell-style [sheet [r c] property value]
  (assoc-in sheet [:grid r c :style property] value))

(defn- unset-cell-style [sheet [r c] property]
  (update-in sheet [:grid r c :style] dissoc property))

(defn- get-cell-style [sheet [r c] property]
  (get-in sheet [:grid r c :style property]))

(defn set-cell-backgrounds [sheet addresses background]
  (reduce #(set-cell-style %1 %2 :background background) sheet addresses))

(defn toggle-cell-bolds [sheet addresses]
  (if (every? #(get-cell-style sheet % :bold) addresses)
    (reduce #(unset-cell-style %1 %2 :bold) sheet addresses)
    (reduce #(set-cell-style %1 %2 :bold true) sheet addresses)))

(declare eval-dep)

(defn eval-cell
  ([address {:keys [grid] :as sheet}]
   (eval-cell address sheet (util/get-cell grid address) false))

  ([address sheet new-content]
   (let [cell (util/get-cell (:grid sheet) address)]
     (eval-cell address sheet (assoc cell :content new-content) true)))

  ([address {:keys [grid depgraph] :as sheet} cell content-changed?]
   (let [cell* (eval-cell* cell sheet)
         clear-addrs (:spilled-into cell)
         cleared-grid (clear-spill grid address)
         unspilled-grid (assoc-in cleared-grid address cell*)
         [grid* evaled-addrs] (if (:matrix cell*)
                                (spill-matrix unspilled-grid address)
                                [unspilled-grid #{address}])
         updated-addrs (set/union evaled-addrs clear-addrs)
         depgraph* (cond-> depgraph content-changed?
                           (deps/update-depgraph
                            [:cell address] cell cell*))
         sheet* (merge sheet {:grid grid* :depgraph depgraph*})
         other-spillers (-> (interested-spillers updated-addrs grid)
                            (disj address))
         deps-to-reval (concat
                        (dependents (map deps/->cell-dep updated-addrs) depgraph)
                        (map deps/->cell-dep other-spillers))]
     (reduce #(eval-dep %2 %1) sheet* deps-to-reval))))

(defn- merge-cell [sheet address merge-with]
  (let [sheet* (if (not= merge-with address)
                 (eval-cell address sheet "")
                 sheet)]
    (set-cell-style sheet* address :merged-with merge-with)))

(defn merge-cells [sheet start end]
  (let [merge-with (util/top-left [start end])
        merged-until (util/bottom-right [start end])
        addresses (mapcat identity (util/addresses-matrix merge-with merged-until))]
    (if (some #(get-cell-style sheet % :merged-with) addresses)
      sheet
      (->  (reduce #(merge-cell %1 %2 merge-with) sheet addresses)
           (set-cell-style merge-with :merged-until merged-until)
           (set-cell-style merge-with :merged-addresses addresses)))))

(defn unmerge-cells [sheet addresses]
  (->> addresses
       (filter #(get-cell-style sheet % :merged-with))
       (reduce
        (fn [sheet* rc]
          (let [merged-with (get-cell-style sheet rc :merged-with)
                merged-addresses (get-cell-style sheet merged-with :merged-addresses)]
            (-> (reduce #(unset-cell-style %1 %2 :merged-with) sheet* merged-addresses)
                (unset-cell-style merged-with :merged-until)
                (unset-cell-style merged-with :merged-addresses))))
        sheet)))

(defn eval-named
  ([name {:keys [bindings] :as sheet}]
   (if-let [value (bindings name)]
     (eval-named name sheet value)
     (errors/undefined-named-ref name)))

  ([name {:keys [bindings] :as sheet} val]
   (-> (let [existing-val (bindings name)
             val* (-> val
                      errors/reset
                      (merge (interpreter/eval-ast (:ast val) sheet)))
             deps-to-reval ((:depgraph sheet) [:named name])]
         (as-> sheet sheet
           (assoc-in sheet [:bindings name] val*)
           (update-in sheet [:depgraph] #(deps/update-depgraph % [:named name] existing-val val*))
           (reduce #(eval-dep %2 %1) sheet deps-to-reval)))
       escalate-bindings-errors)))

(defmulti eval-dep first)

(defmethod eval-dep :cell
  [[_ address] sheet]
  (eval-cell address sheet))

(defmethod eval-dep :named
  [[_ name] sheet]
  (eval-named name sheet))

(defn- eval-grid [sheet]
  (util/reduce-on-sheet-addressed
   #(eval-cell %2 %1)
   sheet))

(defn eval-code
  ;; Suppressing errors so we let the grid evaluate before showing any errors in the code
  ([sheet] (eval-code sheet (:code sheet)))
  ([sheet code]
   (let [res (let [code-ast (parser/parse-statement code)
                   parse-error (parser/error code-ast)]
               (if (string? parse-error)
                 (assoc sheet :code-error parse-error)
                 (-> (reduce (fn [sheet [_ [_ named] expr]]
                               (eval-named named
                                           sheet
                                           (value/from-statement (parser/statement-source code expr)
                                                                 expr)))
                             (dissoc sheet :code-error)
                             (rest code-ast))
                     (assoc :code-ast code-ast))))]
     (escalate-bindings-errors res))))

(defn eval-test [{:keys [tests] :as sheet}]
  (let [tests-ast (trellis-parser/parse-tests tests)]
    (doall
     (map (fn [[_ left-expr right-expr]]
            (let [left-val (interpreter/eval-ast left-expr sheet)
                  right-val (interpreter/eval-ast right-expr sheet)]
              [(= (:scalar left-val)
                  (:scalar right-val))
               (merge {:ast left-expr
                       :content (trellis-parser/trellis-subs tests left-expr)}
                      left-val)
               (merge {:ast right-expr
                       :content (trellis-parser/trellis-subs tests right-expr)}
                      right-val)]))
          (rest tests-ast)))))

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
   :style {:background nil}

   ;; Internal representation of user input
   :ast nil

   ;; Evaluation results
   :scalar nil
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
   :bindings bindings})
