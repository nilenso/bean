(ns bean.core
  (:require [cljs.math :refer [pow]]
            [clojure.set :as set]
            [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
  ;; TODO: Integers are currently just natural numbers
   "
    CellContents = <'='> Expression / Integer / String / Epsilon
    Integer = #'[0-9]+'
    String = #'.+'

    CellRef = #'[A-Z]+' #'[1-9][0-9]*'

    Operation = '+'
    Expression = Value | CellRef | Expression Operation Expression | FunctionInvocation
    FunctionInvocation = FunctionName <'('> FunctionArguments <')'>
    FunctionName = \"concat\"
    FunctionArguments = Epsilon | Expression | Expression <','> FunctionArguments

    Value = Integer / <'\"'> QuotedString <'\"'>
    QuotedString = #'[^\"]+'
    "))

(defn bean-op-+ [left right]
  (if (and (int? left) (int? right))
    (+ left right)
    {:error "Addition only works for Integers"}))

(defn parse [v]
  (insta/parse parser v))

(defn- is-expression? [[node-type & _]]
  (= node-type :Expression))

(defn- ast-result [error-or-val & [dependencies]]
  (if-let [error (:error error-or-val)]
    {:error error :representation (str error)}
    {:value error-or-val
     :representation (str error-or-val)
     :dependencies dependencies}))

(defn- first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn- formulas-map [ast-results f]
  (if-let [referenced-error (first-error ast-results)]
    referenced-error
    (ast-result (apply f (map :value ast-results))
                (apply set/union (keep :dependencies ast-results)))))

(defn get-cell [grid address]
  (if-let [contents (get-in grid address)]
    contents
    {:error (str "Invalid address " address)}))

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

(defn- content->cell
  ([content]
   {:content content
    :ast (parse content)}))

(def ^:private num-alphabets 26)

(defn- a1->rc [a n]
  (let [indexed-a (map vector (reverse a) (range))
        c (reduce (fn [total [alphabet i]]
                    (+ total
                       (* (pow num-alphabets i)
                          (- (.charCodeAt alphabet 0)
                             (dec (.charCodeAt "A" 0))))))
                  0
                  indexed-a)]
    [(dec n) (dec c)]))

(declare eval-cell)

(defn eval-ast [ast cell address grid]
  ; ast goes down, value or an error comes up
  (let [[node-type & [arg :as args]] ast
        eval-sub-ast #(eval-ast % cell address grid)]
    (case node-type
      :CellContents (if arg
                      (eval-sub-ast arg)
                      (ast-result nil))
      :CellRef (let [[_ a n] ast
                     rc (a1->rc a (js/parseInt n))
                     referred-cell (get-cell grid rc)]
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

;; TODO: Is there a better way to return vectors instead of lists
;; for O(1) lookups later.
(defn map-on-matrix [f matrix]
  (vec (map #(vec (map (fn [element] (f element)) %)) matrix)))

(defn map-on-matrix-addressed [f matrix]
  (vec (map-indexed (fn [row-idx row]
                      (vec (map-indexed
                            (fn [col-idx element]
                              (f [row-idx col-idx] element))
                            row)))
                    matrix)))

(defn- eval-cell [cell address grid]
  (-> (eval-ast (:ast cell) cell address grid)
      (ast-result->cell cell)))

(defn- depgraph-add-edge [depgraph parent child]
  (assoc depgraph parent (conj (get depgraph parent #{}) child)))

(defn- depgraph-remove-edge [depgraph parent child]
  (assoc depgraph parent (disj (get depgraph parent) child)))

(defn depgraph [grid]
  (let [edges (->> grid
                   (map-on-matrix-addressed
                    #(for [dependency (:dependencies %2)]
                       {:parent dependency :child %1}))
                   flatten)]
    (reduce
     #(depgraph-add-edge %1 (:parent %2) (:child %2))
     {}
     edges)))

(defn- update-depgraph [depgraph address old-dependencies new-dependencies]
  (as-> depgraph g
    (reduce #(depgraph-remove-edge %1 %2 address) g old-dependencies)
    (reduce #(depgraph-add-edge %1 %2 address) g new-dependencies)))

(defn- evaluate-partial-grid [grid address]
  (assoc-in grid
            address
            (eval-cell (get-cell grid address)
                       address
                       grid)))

(defn evaluate-grid
  ([grid]
   (let [parsed-grid (map-on-matrix content->cell grid)
         evaluated-grid (map-on-matrix-addressed #(eval-cell %2 %1 parsed-grid) parsed-grid)
         depgraph (depgraph evaluated-grid)]
     {:grid evaluated-grid
      :depgraph depgraph}))

  ([address grid depgraph]
   (let [dependents (get depgraph address #{})
         evaluated-grid (evaluate-partial-grid grid address)
         old-dependencies (:dependencies (get-cell grid address))
         new-dependencies (:dependencies (get-cell evaluated-grid address))
         updated-depgraph (update-depgraph depgraph address old-dependencies new-dependencies)]
     ;; Need to re-look at how the depgraph recomposition works.
     (if dependents
       (reduce #(evaluate-grid %2 (:grid %1) (:depgraph %1))
               {:grid evaluated-grid
                :depgraph updated-depgraph}
               dependents)
       {:grid evaluated-grid
        :depgraph updated-depgraph})))

  ([address new-content grid depgraph]
   (evaluate-grid address
                  (assoc-in grid address (content->cell new-content))
                  depgraph)))
