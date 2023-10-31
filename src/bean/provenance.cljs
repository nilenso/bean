(ns bean.provenance
  (:require [bean.util :as util]
            [bean.interpreter :as interpreter]))

(defn- value-proof [v & args]
  (->> args
       (concat [:value v])
       (into [])))

(defn- self-evident [value]
  (value-proof value :self-evident))

(defn combine-op-proof [ast-result lproof op rproof]
  (let [value (:value ast-result)
        matrix (:matrix ast-result)
        lmatrix (:matrix lproof)
        rmatrix (:matrix rproof)]
    (cond
      (and lmatrix rmatrix) {:matrix
                             (util/map-on-matrix-addressed
                              #(value-proof (:value %2) (get-in lmatrix %1) op (get-in rmatrix %1))
                              matrix)}
      lmatrix {:matrix
               (util/map-on-matrix-addressed
                #(value-proof (get-in matrix (conj %1 :value)) %2 op rproof)
                lmatrix)}
      rmatrix {:matrix (util/map-on-matrix-addressed
                #(value-proof (get-in matrix (conj %1 :value)) lproof op %2)
                rmatrix)}
      :else (value-proof value lproof op rproof))))

(declare cell-proof)

(defn ast-proof [ast sheet]
  ;; ast goes down, proof come up
  (let [[node-type & [arg :as args]] ast
        sub-ast-proof #(ast-proof % sheet)
        ast-result #(interpreter/eval-ast ast sheet)
        ast-value #(:value (ast-result))]
    (case node-type
      :CellContents (if arg
                      (sub-ast-proof arg) [])
      :CellRef (let [[_ a n] ast
                     address (util/a1->rc a (js/parseInt n))]
                 (cell-proof address sheet))
      :MatrixRef {:matrix
                  (->> (apply util/matrix-bounds args)
                       (apply util/addresses-matrix)
                       (util/map-on-matrix #(cell-proof % sheet)))}
      :Expression (if (util/is-expression? arg)
                    (let [[left op right] args]
                      (combine-op-proof
                       (ast-result)
                       (sub-ast-proof left)
                       (sub-ast-proof op)
                       (sub-ast-proof right)))
                    (sub-ast-proof arg))
      :Value (sub-ast-proof arg)
      :Integer (self-evident (ast-value))
      :String (self-evident (ast-value))
      :QuotedString (self-evident (ast-value))
      :Operation (self-evident arg))))

(defn- spilled-cell-proof [address cell {:keys [grid] :as sheet}]
  (let [spilled-from (util/get-cell grid (:spilled-from cell))]
    (conj
     [:spill
      {:spilled-from (:spilled-from cell)
       :content (:content spilled-from)
       :address address
       :value (:value cell)
       :relative-address (:relative-address cell)}]
     (get-in
      (:matrix (ast-proof (:ast spilled-from) sheet))
      (:relative-address cell)))))

(defn cell-proof
  "Returns a hiccup style proof tree.
   A proof is a vector of the shape [proof-type proof & dependency-proofs].
  `dependency-proofs` is a list of proofs."
  [address {:keys [grid] :as sheet}]
  (let [cell (util/get-cell grid address)
        error? (:error cell)]
    (when-not error?
      (if (:spilled-from cell)
        (spilled-cell-proof address cell sheet)
        [:cell-ref
         {:address address
          :content (:content cell)
          :value (:value cell)}
         (ast-proof (:ast cell) sheet)]))))
