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

(defn- ast-result->cell [{:keys [error matrix] :as ast-result} cell]
  (merge
   {:content (:content cell)
    :ast (:ast cell)
    :value (:value ast-result)
    :representation (:representation ast-result)}
   (when matrix {:matrix matrix})
   (when error {:error error})))

(defn- first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn- formulas-map [ast-results f]
  (if-let [referenced-error (first-error ast-results)]
    referenced-error
    (ast-result (apply f (map :value ast-results)))))

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

(defn ast->deps [ast]
  (let [[node-type & [arg :as args]] ast]
    (case node-type
      :CellContents (if arg (ast->deps arg) #{})
      :CellRef (let [[_ a n] ast]
                 #{(util/a1->rc a (js/parseInt n))})
      :MatrixRef (let [[start-cell end-cell] args
                       [_ start-a start-n] start-cell
                       [_ end-a end-n] end-cell
                       start-address (util/a1->rc start-a (js/parseInt start-n))
                       end-address (util/a1->rc end-a (js/parseInt end-n))]
                   (->> (addresses-matrix start-address end-address)
                        (mapcat identity)
                        set))
      :Expression (if (is-expression? arg)
                    (let [[left _ right] args]
                      (set/union
                       (ast->deps left)
                       (ast->deps right)))
                    (ast->deps arg))
      #{})))

(defn eval-ast [ast cell grid]
  ; ast goes down, value or an error comes up
  (let [[node-type & [arg :as args]] ast
        eval-sub-ast #(eval-ast % cell grid)]
    (case node-type
      :CellContents (if arg
                      (eval-sub-ast arg)
                      (ast-result nil))
      :CellRef (let [[_ a n] ast
                     address (util/a1->rc a (js/parseInt n))
                     referred-cell (util/get-cell grid address)]
                 (if (:error referred-cell)
                   (ast-result referred-cell)
                   (cell->ast-result referred-cell)))
      :MatrixRef (let [[start-cell end-cell] args
                       [_ start-a start-n] start-cell
                       [_ end-a end-n] end-cell
                       [start-r start-c] (util/a1->rc start-a (js/parseInt start-n))
                       [end-r end-c] (util/a1->rc end-a (js/parseInt end-n))]
                   {:matrix (eval-matrix [start-r start-c] [end-r end-c] grid)})
      :Expression (if (is-expression? arg)
                    (let [[left op right] args]
                      (formulas-map [(eval-sub-ast op)
                                     (eval-sub-ast left)
                                     (eval-sub-ast right)]
                                    #(apply %1 [%2 %3])))
                    (eval-sub-ast arg))
      :Value (eval-sub-ast arg)
      :Integer (ast-result (js/parseInt arg))
      :String (ast-result arg)
      :Operation (ast-result (case arg
                               "+" bean-op-+)))))

(defn eval-cell [cell grid]
  (if (or (not (:spilled-from cell))
          (:matrix cell))
    (-> (eval-ast (:ast cell) cell grid)
        (ast-result->cell cell))
    cell))
