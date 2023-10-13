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
      :MatrixRef {:matrix
                  (->> (apply interpreter/matrix-bounds args)
                       (apply interpreter/addresses-matrix)
                       (util/map-on-matrix #(cell-proof % grid)))}
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

(defn- spilled-cell-proof [cell grid]
  (let [spilled-from (util/get-cell grid (:spilled-from cell))]
    [:spill
     {:spilled-from (:spilled-from cell)
      :content (:content spilled-from)
      :value (:value cell)
      :relative-address (:relative-address cell)}
     (get-in
      (:matrix (ast-proof (:ast spilled-from) grid))
      (:relative-address cell))]))

(defn cell-proof
  "Returns a hiccup style proof tree.
   A proof is a vector of the shape [proof-type proof & dependency-proofs].
  `dependency-proofs` is a list of proofs."
  [address grid]
  (let [cell (util/get-cell grid address)
        error? (:error cell)]
    (when-not error?
      (if (:spilled-from cell)
        (spilled-cell-proof cell grid)
        [:cell-ref
         {:address address
          :content (:content cell)
          :value (:value cell)}
         (ast-proof (:ast cell) grid)]))))

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
