(ns bean.core
  (:require [cljs.math :refer [pow]]
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

(defn- ast-result [error-or-val & dependencies]
  (if-let [error (:error error-or-val)]
    {:error error :representation (str error)}
    {:value error-or-val :representation (str error-or-val) :dependencies dependencies}))

(defn- first-error [ast-results]
  (->> ast-results (filter :error) first))

(defn- formulas-map [ast-results f]
  (if-let [referenced-error (first-error ast-results)]
    referenced-error
    (ast-result (apply f (map :value ast-results))
                (map :dependencies ast-results))))

(defn- get-cell [grid address]
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
    :ast (parse content)
    :value nil})
  ([content cell] (merge cell (content->cell content))))

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
                       (merge {:dependencies [{:dependent address
                                               :support rc}]})
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

(defn depgraph [grid]
  (let [dependencies (->> grid
                          (map-on-matrix :dependencies)
                          flatten
                          (remove nil?))
        add-node (fn [graph parent node]
                   (let [children (get graph parent #{})]
                     (assoc graph parent (conj children node))))
        make-graph (fn [parent-f child-f coll]
                     (reduce #(add-node %1 (parent-f %2) (child-f %2)) {} coll))]
    {:depends-on (make-graph :dependent :support dependencies)
     :supports (make-graph :support :dependent dependencies)}))

(defn evaluate-grid [grid]
  (let [parsed-grid (map-on-matrix content->cell grid)]
    (->> parsed-grid
         (map-on-matrix-addressed #(eval-cell %2 %1 parsed-grid)))))
