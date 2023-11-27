(ns bean.ui.provenance
  (:require [bean.ui.util :as util]
            [bean.provenance :as provenance]
            [bean.parser.parser :as parser]
            [clojure.string :as string]))

(defn subtree-proof-sentence
  [spacer [_ proof & dependency-proofs]]
  (let [[res que]
        (reduce
         (fn [[s queue] subproof]
           (let [[proof-type p & rest] subproof]
             [(str s (case proof-type
                       :value (str p " ")
                       :spill (str (apply util/rc->a1 (:address p)) " ")
                       :cell-ref (str (apply util/rc->a1 (:address p)) " ")))
              (if (coll? (first rest))
                (conj queue subproof)
                queue)]))
         (case _
           :value [(str proof " is ") []]
           :spill [(str (apply util/rc->a1 (:address proof)) " is ") []]
           :cell-ref [(str (apply util/rc->a1 (:address proof)) " is ") []])
         dependency-proofs)]
    (if (first que)
      (let [new-spacer (str spacer "  ")]
        (str res "\n" new-spacer "where " (string/join (str  "\n" new-spacer "and ") (map #(subtree-proof-sentence new-spacer %) que))))
      res)))

(defn sentence-proof 
  ([expression sheet]
   (let [proof-tree (provenance/ast-proof
                     (parser/parse (str "=" expression))
                     sheet)
         [proof-type proof & dependency-proofs] proof-tree
         sproof (case proof-type
                  :value (str "it's " proof)
                  :spill (str "it's " (:value proof))
                  :cell-ref (str (apply util/rc->a1 (:address proof)) " is " (:value proof)))]
     (if (coll? (first dependency-proofs))
       (str sproof " because\n" (subtree-proof-sentence "" proof-tree))
       sproof))))
