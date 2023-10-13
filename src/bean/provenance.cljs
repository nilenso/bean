(ns bean.provenance
  (:require [bean.util :as util]
            [bean.interpreter :as interpreter]))

;; workings can be
;; - a list: [{value-workings}]
;; - {:ref {:address ... (a cell proof)}}
;; - {self-evident true}

(defn- proof [value workings]
  {:value value
   :workings workings})

(defn- self-evident [value]
  {:value value
   :workings {:self-evident true}})

(declare cell-proof)

(defn- ast-workings [ast grid]
  ;; ast goes down, workings come up
  (let [[node-type & [arg :as args]] ast
        sub-ast-workings #(ast-workings % grid)]
    (case node-type
      :CellContents (if arg
                      (sub-ast-workings arg)
                      {:workings []})
      :CellRef (let [[_ a n] ast
                     address (util/a1->rc a (js/parseInt n))]
                 (proof
                  (:value (util/get-cell grid address))
                  {:ref (cell-proof address grid)}))
      :Expression (if (interpreter/is-expression? arg)
                    (let [[left op right] args]
                      (proof
                       (:value (interpreter/eval-ast ast grid))
                       [(sub-ast-workings left)
                        (sub-ast-workings op)
                        (sub-ast-workings right)]))
                    (sub-ast-workings arg))
      :Value (sub-ast-workings arg)
      :Integer (self-evident (:value (interpreter/eval-ast ast grid)))
      :String (self-evident (:value (interpreter/eval-ast ast grid)))
      :QuotedString (self-evident (:value (interpreter/eval-ast ast grid)))
      :Operation (self-evident arg))))

(defn cell-proof
  [address grid]
  (let [cell (util/get-cell grid address)
        error? (:error cell)]
    (when-not error?
      {:address address
       :content (:content cell)
       :value (:value cell)
       :workings (:workings (ast-workings (:ast cell) grid))})))

