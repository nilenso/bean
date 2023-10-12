(ns bean.provenance
  (:require [bean.util :as util]
            [bean.interpreter :as interpreter]))

(defn- proof [value workings]
  (if workings
    {:value value
     :workings workings}
    {:self-evident value}))

(defn- ast-workings [ast grid]
  ;; ast goes down, workings comes up
  (let [[node-type & [arg :as args]] ast
        sub-ast-workings #(ast-workings % grid)]
    (case node-type
      :CellContents (if arg
                      (sub-ast-workings arg)
                      nil)
      :Expression (if (interpreter/is-expression? arg)
                    (let [[left op right] args]
                      (proof
                       (:value (interpreter/eval-ast ast grid))
                       [(sub-ast-workings left)
                        (sub-ast-workings op)
                        (sub-ast-workings right)]))
                    (sub-ast-workings arg))
      :Value (sub-ast-workings arg)
      :Integer (proof (:value (interpreter/eval-ast ast grid)) nil)
      :String (proof (:value (interpreter/eval-ast ast grid)) nil)
      :QuotedString (proof (:value (interpreter/eval-ast ast grid)) nil)
      :Operation (proof arg nil))))

(defn cell-proof
  [address grid]
  (let [cell (util/get-cell grid address)]
    (merge
     {:address address 
      :value (:value cell)
      :content (:content cell) 
      :workings (ast-workings (:ast cell) grid)})))

