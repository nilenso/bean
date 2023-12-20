(ns bean.table
  (:require [bean.util :as util]))

;; # Table impl todo list
;; 
;; store cells under a label
;; store labels on a cell
;; 
;; ## lookups
;; address by name
;; name by address
;; table by name
;; table by address
;; labels by row/column
;; 
;; ## Recompute
;; each labelled cell is a dependency of the table (eval-dep :table)
;; labelled cells are a dependency of the label's address
;;   (the labelled address depends on the labelled cells)
;; formulas using a label are dependencies of the label
;;   (formulas are dependent on the label)
;; ## Expression rewriting
;; labels actually refer to the address of the label somehow?

;; TODO: disallow empty labels
(defn make-table [grid start end direction]
  {:name ""
   :start-address start
   :end-address end
   :labels
   (into {}
         (for [address (util/top-addresses start end)]
           [address
            {:name (get-in grid (conj address :representation))
             :span 1 ;; TODO: the span should come from the sheet when merged cells become a thing
             :direction direction}]))
  ;; breaks with two labels of same name
   :label-addresses-by-name
   (into
    {}
    (for [address (util/top-addresses start end)]
      [(get-in grid (conj address :representation)) address]))})

;; TODO: this should account for spans when we add merged cells
(defn top-lookup-label [label table grid]
  ;; breaks with two labels of same name
  (let [start-address (get-in table [:label-addresses-by-name label])
        [row col] start-address
        [table-end-row _] (:end-address table)
        end-address (or
                     (take-while
                      #(not (get-in table [:labels %2]))
                      (range row table-end-row))
                     [table-end-row (+ (dec (:span label)) col)])]
    (util/sub-grid grid [(inc row) col] end-address)))

(defn left-lookup-label [label table grid]
  (let [start-address (get-in table [:label-addresses-by-name label])
        [row col] start-address
        [_ table-end-col] (:end-address table)
        end-address (or
                     (take-while
                      #(not (get-in table [:labels %2]))
                      (range col table-end-col))
                     [(+ (dec (:span label)) row) table-end-col])]
    (util/sub-grid grid [row (inc col)] end-address)))

