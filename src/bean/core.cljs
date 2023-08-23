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

(defn- error-or-value [result]
  (if (:error result)
    result
    {:value result :representation (str result)}))

(defn- formulas-map [cells f]
  (if-let [error (some #(when (not (nil? %)) %) (map :error cells))]
    {:error error}
    (error-or-value (apply f (map :value cells)))))

(defn- get-cell [grid address]
  (if-let [contents (get-in grid address)]
    contents
    {:error (str "Invalid address " address)}))

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
  (let [eval-sub-ast #(eval-ast % cell address grid)
        [node-type & [arg :as args]] ast
        return-value #(do {:value % :representation (str %)})
        return-error #(do {:error %})]
    (case node-type
      :CellContents (merge cell (if arg
                                  (eval-sub-ast arg)
                                  (return-value nil)))
      :CellRef (let [[_ a n] ast
                     rc (a1->rc a (js/parseInt n))
                     referred-cell (get-cell grid rc)
                     error (:error referred-cell)]
                 (if error
                   (return-error error)
                   (select-keys
                    (eval-cell referred-cell rc grid)
                    [:value :error])))
      :Expression (if (is-expression? arg)
                    (let [[left op right] args]
                      (formulas-map [(eval-sub-ast op)
                                     (eval-sub-ast left)
                                     (eval-sub-ast right)]
                                    #(apply %1 [%2 %3])))
                    (eval-sub-ast arg))
      :Value (eval-sub-ast arg)
      :Integer (return-value (js/parseInt arg))
      :String (return-value arg)
      :Operation (return-value (case arg
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

(defn- content->cell [content]
  {:content content
   :ast (parse content)
   :value nil
;; :error nil
   :representation ""})

(defn eval-cell [cell address grid]
  (eval-ast (:ast cell) cell address grid))

(defn evaluate-grid [grid]
  (let [parsed-grid (map-on-matrix content->cell grid)]
    (->> parsed-grid
         (map-on-matrix-addressed #(eval-cell %2 %1 parsed-grid)))))
