(ns bean.interpreter
  (:require [clojure.set :as set]
            [bean.util :as util]))

(defn- is-expression? [[node-type & _]]
  (= node-type :Expression))

(defn- ast-result [error-or-val]
  (if-let [error (:error error-or-val)]
    {:error error :representation (str error)}
    {:value error-or-val
     :representation (str error-or-val)}))

(defn- cell->ast-result [cell]
  (select-keys cell [:value :error :representation]))

(defn- ast-result->cell [{:keys [error matrix ctx] :as ast-result} cell]
  (merge
   {:content (:content cell)
    :ast (:ast cell)
    :value (:value ast-result)
    :representation (:representation ast-result)}
   (when matrix {:matrix matrix})
   (when ctx {:ctx ctx})
   (when error {:error error})))

(defn- first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn bean-op-+ [left right]
  (if (and (int? left) (int? right))
    (+ left right)
    {:error "Addition only works for Integers"}))

(defn- addresses-matrix
  [[start-row start-col] [end-row end-col]]
  (for [r (range start-row (inc end-row))]
    (for [c (range start-col (inc end-col))]
      [r c])))

(defn- eval-matrix [start-address end-address grid]
  (util/map-on-matrix
   #(util/get-cell grid %)
   (addresses-matrix start-address end-address)))

(defn- apply-values
  ([f ast-results]
   (if-let [referenced-error (first-error ast-results)]
     referenced-error
     (ast-result (apply f (map :value ast-results)))))
  ([f ast-results matrix]
   {:matrix
    (util/map-on-matrix
     #(apply-values f (conj ast-results %))
     matrix)}))

(defn- dim [matrix]
  [(count matrix) (count (first matrix))])

(defn- matrix-op-matrix [lmatrix op rmatrix]
  (if (= (dim lmatrix) (dim rmatrix))
    {:matrix
     (mapv (partial mapv
                    (fn [l-el r-el]
                      (apply-values
                       #(apply %1 [%2 %3])
                       [op l-el r-el])))
           lmatrix
           rmatrix)}
    {:error "Matrices should be same size."
     :representation "Matrices should be same size."}))

(defn apply-op [op left right]
  (let [lmatrix (:matrix left)
        rmatrix (:matrix right)]
    (cond
      (and lmatrix rmatrix) (matrix-op-matrix lmatrix op rmatrix)
      lmatrix (apply-values #(apply %1 [%2 %3]) [op right] lmatrix)
      rmatrix (apply-values #(apply %1 [%3 %2]) [op left] rmatrix)
      :else (apply-values #(apply %1 [%2 %3]) [op left right]))))

(defn matrix-bounds [start-ref end-ref]
  (let [[_ start-a start-n] start-ref
        [_ end-a end-n] end-ref
        start-address (util/a1->rc start-a (js/parseInt start-n))
        end-address (util/a1->rc end-a (js/parseInt end-n))]
    [start-address end-address]))

(defn ast->deps [ast]
  (let [[node-type & [arg :as args]] ast]
    (case node-type
      :CellContents (if arg (ast->deps arg) #{})
      :CellRef (let [[_ a n] ast]
                 #{(util/a1->rc a (js/parseInt n))})
      :MatrixRef (->> (apply matrix-bounds args)
                      (apply addresses-matrix)
                      (mapcat identity)
                      set)
      :Expression (if (is-expression? arg)
                    (let [[left _ right] args]
                      (set/union
                       (ast->deps left)
                       (ast->deps right)))
                    (ast->deps arg))
      #{})))

(defn eval-ast [ast cell grid ctx]
  ; ast goes down, value or an error comes up (with ctx)
  ;; value: 1 "fewfe" {:function ast}, matrix, error,
  (let [[node-type & [arg :as args]] ast
        eval-sub-ast #(eval-ast % cell grid ctx)
        eval-matrix* #(eval-matrix %1 %2 grid)]
    (case node-type
      :CellContents (if arg
                      (eval-sub-ast arg)
                      (ast-result nil)) 
      :Program (for [[_ ] args]
                 (eval-sub-ast args))
      :Statement (if arg
                   (eval-sub-ast arg)
                   (ast-result nil))
      :LetExpression (let [[_ [_ name] expression] args
                           expr-value (-> expression eval-sub-ast ast-result)
                           new-ctx (assoc ctx name expr-value)]
                       (assoc expr-value :ctx new-ctx))
      :FunctionDefinition (let [_ args] (ast-result {:function args}))
      :Name (let [[_ name] args] (get ctx name))
      :CellRef (let [[_ a n] ast
                     address (util/a1->rc a (js/parseInt n))
                     referred-cell (util/get-cell grid address)
                     cell-exists? (not (:error referred-cell))]
                 (cond-> referred-cell
                   cell-exists? cell->ast-result
                   (not cell-exists?) ast-result))
      :MatrixRef {:matrix (->> args
                               (apply matrix-bounds)
                               (apply eval-matrix*))}
      :Expression (if (is-expression? arg)
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
                               "+" bean-op-+)))))

(defn eval-cell [cell grid ctx]
  (-> (eval-ast (:ast cell) cell grid ctx)
      (ast-result->cell cell)))
