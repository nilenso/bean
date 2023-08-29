(ns bean.interpreter
  (:require [clojure.set :as set]
            [bean.util :as util]))

(defn- ast-result [error-or-val & [dependencies]]
  (if-let [error (:error error-or-val)]
    {:error error :representation (str error)}
    {:value error-or-val
     :representation (str error-or-val)
     :dependencies dependencies}))

(defn- is-expression? [[node-type & _]]
  (= node-type :Expression))

(defn- first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn- cell->ast-result [cell]
  (select-keys cell [:value :error :representation :dependencies]))

(defn- ast-result->cell [{:keys [error dependencies] :as ast-result} cell]
  (merge
   {:content (:content cell)
    :ast (:ast cell)
    :value (:value ast-result)
    :representation (:representation ast-result)}
   (when dependencies {:dependencies dependencies})
   (when error {:error error})))

(defn- formulas-map [ast-results f]
  (if-let [referenced-error (first-error ast-results)]
    referenced-error
    (ast-result (apply f (map :value ast-results))
                (apply set/union (keep :dependencies ast-results)))))

(declare eval-ast)

(defn eval-cell [cell address grid]
  (-> (eval-ast (:ast cell) cell address grid)
      (ast-result->cell cell)))

(defn bean-op-+ [left right]
  (if (and (int? left) (int? right))
    (+ left right)
    {:error "Addition only works for Integers"}))

(defn eval-ast [ast cell address grid]
  ; ast goes down, value or an error comes up
  (let [[node-type & [arg :as args]] ast
        eval-sub-ast #(eval-ast % cell address grid)]
    (case node-type
      :CellContents (if arg
                      (eval-sub-ast arg)
                      (ast-result nil))
      :CellRef (let [[_ a n] ast
                     rc (util/a1->rc a (js/parseInt n))
                     referred-cell (util/get-cell grid rc)]
                 (if (:error referred-cell)
                   (ast-result referred-cell)
                   (-> (eval-cell referred-cell rc grid)
                       (merge {:dependencies #{rc}})
                       cell->ast-result)))
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
