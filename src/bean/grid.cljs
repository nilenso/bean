(ns bean.grid
  (:require [bean.area :as area]
            [bean.code-errors :as code-errors]
            [bean.deps :as deps]
            [bean.errors :as errors]
            [bean.functions :as functions]
            [bean.interpreter :as interpreter]
            [bean.parser.parser :as parser]
            [bean.parser.trellis-parser :as trellis-parser]
            [bean.frames :as frames]
            [bean.util :as util]
            [bean.value :as value]
            [clojure.set :as set]
            [clojure.string]))

(def default-bindings
  {"concat" {:scalar functions/bean-concat
             :representation "f"}
   "get" {:scalar functions/bean-get
          :representation "f"}
   "vget" {:scalar functions/bean-vget
           :representation "f"}
   "hget" {:scalar functions/bean-hget
           :representation "f"}
   "frame" {:scalar functions/bean-frame
            :representation "f"}
   "filter" {:scalar functions/bean-filter
             :representation "f"}
   "reduce" {:scalar functions/bean-reduce
             :representation "f"}
   "row" {:scalar functions/bean-row
          :representation "f"}
   "col" {:scalar functions/bean-col
          :representation "f"}
   "transpose" {:scalar functions/bean-transpose
                :representation "f"}
   "match" {:scalar functions/bean-match
            :representation "f"}
   "error" {:scalar functions/bean-error
            :representation "f"}})

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
              [r c] (util/offset spilled-from relative-address)
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
              address* (util/offset spiller relative-address)
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
      (->> spillage (map #(util/offset spiller (:relative-address %))) set))

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

(defn eval-content [sheet content]
  (interpreter/eval-ast (parser/parse content) sheet))

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
      :code-in-editor code
      :tests tests
      :bindings default-bindings
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

(defn all-bold? [sheet addresses]
  (every? #(get-cell-style sheet % :bold) addresses))

(defn toggle-cell-bolds [sheet addresses]
  (if (all-bold? sheet addresses)
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
         sheet* (merge (frames/expand-frames sheet address)
                       {:grid grid* :depgraph depgraph*})
         other-spillers (-> (interested-spillers updated-addrs grid)
                            (disj address))
         deps-to-reval (concat
                        (dependents (map deps/->cell-dep updated-addrs) depgraph)
                        (map deps/->cell-dep other-spillers))]
     (reduce #(eval-dep %2 %1) sheet* deps-to-reval))))

;; Temporary hack until we figure out how to reval
;; frame queries
(defn eval-sheet-a-few-times [sheet]
  (let [addresses (->> (:grid sheet)
                       (util/map-on-matrix-addressed
                        (fn [address item]
                          (when (and (not= (:content item) "")
                                     (not (nil? (:content item)))
                                     ;; only reval formulas
                                     (= (first (:content item)) "="))
                            address)))
                       (mapcat identity)
                       (remove nil?))]
    (reduce
     (fn [sheet* _] (reduce #(eval-cell %2 %1) sheet* addresses)) sheet (range 3))))

(defn update-cell-content [address sheet content]
  (if (= (:content (util/get-cell (:grid sheet) address)) content "")
    sheet
    (eval-sheet-a-few-times (eval-cell address sheet content))))

(defn- merge-cell [sheet address merge-with]
  (let [sheet* (if (not= merge-with address)
                 (eval-cell address sheet "")
                 sheet)]
    (set-cell-style sheet* address :merged-with merge-with)))

(defn can-merge? [sheet addresses]
  (let [merged-already (map #(get-cell-style sheet % :merged-with) addresses)
        all-merged-addresses (set
                              (mapcat #(get-cell-style
                                        sheet
                                        (get-cell-style sheet % :merged-with)
                                        :merged-addresses) merged-already))]
    (and (every? #(get addresses %) all-merged-addresses)
         (> (count addresses) 1))))

(defn merge-cells [sheet {:keys [start end] :as area}]
  (let [addresses (area/area->addresses area)]
    (if (can-merge? sheet addresses)
      (-> (reduce #(merge-cell %1 %2 start) sheet addresses)
          (set-cell-style start :merged-until end)
          (set-cell-style start :merged-addresses addresses)
          (frames/merge-labels start addresses))
      sheet)))

(defn can-unmerge? [sheet addresses]
  (some #(get-cell-style sheet % :merged-with) addresses))

(defn unmerge-cells [sheet addresses]
  (->> addresses
       (filter #(get-cell-style sheet % :merged-with))
       (reduce
        (fn [sheet* rc]
          (let [merged-with (get-cell-style sheet rc :merged-with)
                merged-addresses (get-cell-style sheet merged-with :merged-addresses)]
            (as-> sheet* sheet*
              (reduce #(unset-cell-style %1 %2 :merged-with) sheet* merged-addresses)
              (reduce #(unset-cell-style %1 %2 :merged-addresses) sheet* merged-addresses)
              (reduce #(unset-cell-style %1 %2 :merged-until) sheet* merged-addresses))))
        sheet)))

;; untested and slightly weird interface, exists for pasting
;; many cells and handling merged cells etc.
(defn update-cells-bulk [sheet {:keys [start]} addressed-attrs]
  (->> addressed-attrs
       (map #(do [(util/offset (first %) start) (second %)]))
       (reduce
        (fn [sheet* [address attrs]]
          (let [existing-cell (util/get-cell (:grid sheet*) address)
                new-cell (-> existing-cell
                             (assoc :content (:content attrs))
                             (assoc :style (:style attrs)))
                new-sheet (eval-cell address sheet* new-cell true)]
            (if (:merge-until attrs)
              (merge-cells new-sheet
                           {:start address
                            :end (util/offset (:merge-until attrs) start)})
              new-sheet)))
        (unmerge-cells sheet (map #(util/offset % start) (keys addressed-attrs))))
       eval-sheet-a-few-times))

(defn make-frame [sheet frame-name addresses]
  (-> (frames/make-frame sheet frame-name addresses)
      eval-sheet-a-few-times))

(defn- move-merged-cell [cell move-by]
  (cond-> cell
    (get-in cell [:style :merged-until])
    (update-in [:style :merged-until] #(util/offset % move-by))

    (get-in cell [:style :merged-addresses])
    (update-in [:style :merged-addresses] #(map (fn [address] (util/offset address move-by)) %))

    (get-in cell [:style :merged-with])
    (update-in [:style :merged-with] #(util/offset % move-by))))

(defn move-cells
  [sheet {:keys [start end]} move-to]
  (let [addresses (area/area->addresses {:start start :end end})
        move-by (util/distance start move-to)
        empty-cell {:content "" :representation ""}
        cleared-sheet (reduce
                       #(assoc-in %1 (flatten [:grid %2]) empty-cell)
                       sheet addresses)
        unspilled-addresses (filter
                             #(let [cell (util/get-cell (:grid sheet) %)]
                                (or (:spilled-into cell)
                                    (nil? (:spilled-from cell))))
                             addresses)]
    (reduce #(assoc-in %1
                       (flatten [:grid (util/offset move-by %2)])
                       (move-merged-cell (util/get-cell (:grid sheet) %2) move-by))
            cleared-sheet
            unspilled-addresses)))

(defn add-frame-labels [sheet frame-name addresses dirn]
  (-> sheet
      (frames/add-labels frame-name addresses dirn)
      eval-sheet-a-few-times))

(defn remove-frame-labels [sheet frame-name addresses]
  (-> sheet
      (frames/remove-labels frame-name addresses)
      eval-sheet-a-few-times))

(defn mark-skip-cells [sheet frame-name addresses]
  (-> (frames/mark-skipped sheet frame-name addresses)
      eval-sheet-a-few-times))

(defn unmark-skip-cells [sheet frame-name addresses]
  (-> (frames/unmark-skipped sheet frame-name addresses)
      eval-sheet-a-few-times))

(defn pasted-area [pasted-at addresses]
  (let [{:keys [start end]} (area/addresses->area addresses)]
    {:start (util/offset start pasted-at)
     :end (util/offset end pasted-at)}))

(defn resize-frame [sheet frame-name area]
  (if-let [sheet* (frames/resize-frame sheet frame-name area)]
    (eval-sheet-a-few-times sheet*)
    sheet))

(defn rename-frame [sheet old-name new-name]
  (if-let [frame (get-in sheet [:frames old-name])]
    (-> sheet
        (update :frames dissoc old-name)
        (assoc-in [:frames new-name] frame)
        eval-sheet-a-few-times)
    sheet))

(defn move-frame [sheet frame-name move-to]
  (let [{:keys [start end]} (get-in sheet [:frames frame-name])
        new-area {:start move-to :end (util/offset move-to (util/distance start end))}]
    ;; check for overlaps first
    (-> (move-cells sheet {:start start :end end} move-to)
        (frames/move-frame frame-name new-area)
        eval-sheet-a-few-times)))

(defn clear-area [sheet {:keys [start end]}]
  (->> (util/addresses-matrix start end)
       (mapcat identity)
       (map #(do [% {:content "" :style {}}]))
       (into {})
       (update-cells-bulk sheet start)))

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
  ([sheet] (eval-code sheet (:code-in-editor sheet)))
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
   :frames frames

   ;; Evaluated fields
   :depgraph depgraph
   :bindings bindings})
