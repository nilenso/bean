(ns bean.interpreter
  (:require [bean.functions :as functions]
            [bean.util :as util]
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
   (when error {:error error})))

(defn- first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn- eval-matrix [start-address end-address grid]
  (util/map-on-matrix
   #(util/get-cell grid %)
   (util/addresses-matrix start-address end-address)))

(defn- apply-results
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

(defn apply-op [op left right]
  (let [lmatrix (:matrix left)
        rmatrix (:matrix right)]
    (cond
      (and lmatrix rmatrix) (matrix-op-matrix lmatrix op rmatrix)
      lmatrix (apply-results #(apply %1 [%2 %3]) [op right] lmatrix)
      rmatrix (apply-results #(apply %1 [%3 %2]) [op left] rmatrix)
      :else (apply-results #(apply %1 [%2 %3]) [op left right]))))

(def global-ctx
  {"concat" {:scalar functions/bean-concat
             :representation "f"}
   "error" {:scalar functions/bean-error
            :representation "f"}})

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
                (get global-ctx arg)
                (errors/undefined-named-ref arg))
      :FunctionDefinition (fn-result arg)
      :FunctionInvocation (apply-f sheet
                                   (eval-sub-ast arg)
                                   (map eval-sub-ast (rest args)))
      :Expression (if (util/is-expression? arg)
                    (let [[left op right] args]
                      (apply-op (eval-sub-ast op)
                                (eval-sub-ast left)
                                (eval-sub-ast right)))
                    (eval-sub-ast arg))
      :Value (eval-sub-ast arg)
      :Integer (ast-result (js/parseInt arg))
      :String (ast-result arg)
      :QuotedString (ast-result arg)
      :Operation (ast-result (case arg
                               "+" operators/bean-op-+
                               "*" operators/bean-op-*)))))

(defn- apply-f [sheet f params]
  (if (fn? (:scalar f))
    ((:scalar f) params)
    (eval-ast (:scalar f)
              (update-in sheet
                         [:bindings]
                         #(merge % (into {} (map vector ["x" "y" "z"] params)))))))

(defn eval-cell [cell sheet]
  (-> (eval-ast (:ast cell) sheet)
      (ast-result->cell cell)))
