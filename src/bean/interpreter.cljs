(ns bean.interpreter
  (:require [bean.util :as util]
            [bean.errors :as errors]
            [bean.operators :as operators]))

(defn- ast-result [error-or-val]
  (if-let [error (:error error-or-val)]
    (errors/stringified-error error)
    {:scalar error-or-val
     :representation (str error-or-val)}))

(defn- fn-result [f]
  {:scalar f
   :representation ""})

(defn- cell->ast-result [cell]
  (select-keys cell [:scalar :error :representation]))

(defn- ast-result->cell [{:keys [error matrix] :as ast-result} cell]
  (merge
   {:content (:content cell)
    :ast (:ast cell)
    :scalar (:scalar ast-result)
    :representation (:representation ast-result)}
   (when matrix {:matrix matrix})
   (when error {:error error})
   (when (:style cell) {:style (:style cell)})))

(defn first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn eval-matrix [start-address end-address grid]
  (util/map-on-matrix
   #(util/get-cell grid %)
   (util/addresses-matrix start-address end-address)))

(defn apply-results
  ([f ast-results]
   (if-let [referenced-error (first-error ast-results)]
     referenced-error
     (ast-result (apply f (map :scalar ast-results)))))
  ([f ast-results matrix]
   {:matrix
    (util/map-on-matrix
     #(apply-results f (conj ast-results %))
     matrix)}))

(defn- dim [matrix]
  [(count matrix) (count (first matrix))])

(defn- matrix-op-matrix [lmatrix op rmatrix]
  (if (= (dim lmatrix) (dim rmatrix))
    {:matrix
     (mapv (partial mapv
                    (fn [l-el r-el]
                      (apply-results
                       #(apply %1 [%2 %3])
                       [op l-el r-el])))
           lmatrix
           rmatrix)}
    (errors/matrix-size-mismatch-error)))

(defn- scalar-op-matrix [op scalar matrix]
  (apply-results #(apply %1 [%2 %3]) [op scalar] matrix))

(defn- matrix-op-scalar [op scalar matrix]
  (apply-results #(apply %1 [%3 %2]) [op scalar] matrix))

(defn apply-op [op left right]
  (let [lmatrix (:matrix left)
        rmatrix (:matrix right)]
    (cond
      (and lmatrix rmatrix) (matrix-op-matrix lmatrix op rmatrix)
      ;; doesnt work for noncommutative operators
      lmatrix (matrix-op-scalar op right lmatrix)
      rmatrix (scalar-op-matrix op left rmatrix)
      :else (apply-results #(apply %1 [%2 %3]) [op left right]))))

(declare apply-f)

(defn eval-ast [ast {:keys [grid bindings] :as sheet}]
  ; ast goes down, result or an error comes up
  (let [[node-type & [arg :as args]] ast
        eval-sub-ast #(eval-ast % sheet)
        eval-matrix* #(eval-matrix %1 %2 grid)]
    (case node-type
      :CellContents (if arg
                      (eval-sub-ast arg)
                      (ast-result nil))
      :CellRef (let [[_ a n] ast
                     address (util/a1->rc a (js/parseInt n))
                     referred-cell (util/get-cell grid address)]
                 (if (errors/get-error referred-cell)
                   (ast-result referred-cell)
                   (cell->ast-result referred-cell)))
      :MatrixRef {:matrix (->> args
                               (apply util/matrix-bounds)
                               (apply eval-matrix*))}
      :Name (or (get bindings arg)
                (errors/undefined-named-ref arg))
      :FunctionDefinition (fn-result arg)
      :FunctionInvocation (apply-f sheet
                                   (eval-sub-ast arg)
                                   (rest args))
      :FrameLookup (let [[_ frame-name] arg]
                     (eval-sub-ast
                      [:FunctionInvocation
                       [:Name "frame"]
                       [:Expression [:QuotedString frame-name]]]))
      :FunctionChain (let [[expr [node-type*
                                  [_ name* :as name-node]
                                  & fn-args]] args]
                       (eval-sub-ast
                        (case node-type*
                          :LabelLookup
                          [:FunctionInvocation
                           [:Name "get"] expr
                           [:Expression [:Value [:QuotedString name*]]]]
                          :FunctionInvocation
                          (concat [:FunctionInvocation name-node expr] fn-args))))
      :Expression (if (util/is-expression? arg)
                    (let [[left op right] args]
                      (apply-op (eval-sub-ast op)
                                (eval-sub-ast left)
                                (eval-sub-ast right)))
                    (eval-sub-ast arg))
      :Value (eval-sub-ast arg)
      :Number (ast-result (js/Number.parseFloat arg))
      :String (ast-result arg)
      :QuotedString (ast-result arg)
      :Operation (ast-result (case arg
                               "+" operators/bean-op-+
                               "-" operators/bean-op-minus
                               "/" operators/bean-op-div
                               "*" operators/bean-op-*
                               "<" operators/bean-op-<
                               ">" operators/bean-op->
                               "=" operators/bean-op-=)))))

(defn eval-asts [sheet asts]
  (map (fn [arg-ast] (eval-ast arg-ast sheet)) asts))

(defn- bind-to-xyz [values]
  (into {} (map vector ["x" "y" "z"] values)))

(defn- apply-system-f [sheet f args asts]
  (if asts
    (f sheet args asts)
    (f sheet args)))

(defn- apply-user-f [sheet f args]
  (eval-ast f (update-in sheet [:bindings]
                         merge (bind-to-xyz args))))

(defn apply-f-args [sheet f args & [asts]]
  (let [fn-ast (:scalar f)]
    (cond
      (:error f) f
      (fn? fn-ast) (apply-system-f sheet fn-ast args asts)
      :else (apply-user-f sheet fn-ast args))))

(defn apply-f [sheet f asts]
  (let [args (eval-asts sheet asts)]
    (apply-f-args sheet f args asts)))

(defn eval-cell [cell sheet]
  (-> (eval-ast (:ast cell) sheet)
      (ast-result->cell cell)))
