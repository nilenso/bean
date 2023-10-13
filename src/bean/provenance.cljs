(ns bean.provenance
  (:require [bean.util :as util]
            [bean.interpreter :as interpreter]))

(defn- self-evident [value]
  [:value value :self-evident])

(declare cell-proof)

(defn- ast-proof [ast grid]
  ;; ast goes down, proof come up
  (let [[node-type & [arg :as args]] ast
        sub-ast-proof #(ast-proof % grid)
        ast-value #(:value (interpreter/eval-ast ast grid))]
    (case node-type
      :CellContents (if arg
                      (sub-ast-proof arg) [])
      :CellRef (let [[_ a n] ast
                     address (util/a1->rc a (js/parseInt n))]
                 (cell-proof address grid))
      :Expression (if (interpreter/is-expression? arg)
                    (let [[left op right] args]
                      [:value
                       (ast-value)
                       (sub-ast-proof left)
                       (sub-ast-proof op)
                       (sub-ast-proof right)])
                    (sub-ast-proof arg))
      :Value (sub-ast-proof arg)
      :Integer (self-evident (ast-value))
      :String (self-evident (ast-value))
      :QuotedString (self-evident (ast-value))
      :Operation (self-evident arg))))

(defn cell-proof
  "Returns a hiccup style proof tree.
   A proof is a vector of the shape [proof-type proof & dependency-proofs].
  `dependency-proofs` is a list of proofs."
  [address grid]
  (let [cell (util/get-cell grid address)
        error? (:error cell)]
    (when-not error?
      [:cell-ref
       {:address address
        :content (:content cell)
        :value (:value cell)}
       (ast-proof (:ast cell) grid)])))

(defn explain
  "Linearise cell-proof"
  [acc proof-tree]
  (let [[proof-type proof & dependency-proofs] proof-tree]
    (concat
     acc
     [(case proof-type
        :cell-ref proof
        :value proof)]
     (if (= :self-evident (first dependency-proofs))
       [(str proof " is self-evident")]
       (reduce explain [] dependency-proofs)))))
