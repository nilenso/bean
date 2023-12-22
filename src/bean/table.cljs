(ns bean.table
  (:require [bean.util :as util]
            [malli.core :as m]
            [clojure.set :as set]))

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

(defn- insert-table [sheet start end])

(defn- table-name [[r1 c1] [r2 c2]]
  (str (util/i->a c1) r1 "-" (util/i->a c2) r2))

(defn is-top-left-of [[r1 c1] [r2 c2]]
  (and (<= r1 r2) (<= c1 c2)))

(def ^:private r first)
(def ^:private c second)

(defn- outside? [cell [top-left bottom-right :as _bounds]]
  (or
   (< (r cell) (r top-left))
   (> (r cell) (r bottom-right))
   (< (c cell) (c top-left))
   (> (c cell) (c bottom-right))))

(defn- inside? [cell bounds]
  (not (outside? cell bounds)))

(defn no-tables-intersect [tables start end]
  true)

(defn- gen-id []
  (str (random-uuid)))

(defn make-table- [sheet start end]
  (prn :no-tab-intersect (no-tables-intersect sheet start end))
  (prn :is-top-left-of (is-top-left-of start end))
  (if (and (no-tables-intersect sheet start end)
           (is-top-left-of start end))
    (let [id (gen-id)
          table {:id id
                 :name (table-name start end)
                 :start start
                 :end end
                 :labels {}}]
      (prn :insprettable table)
      [(update-in sheet [:tables] #(assoc % id table)) table])
    (do
      (prn :nosstable nil)
      [sheet nil])))

(defn- insert-label [sheet table-id label-type cell-address]
  (let [id (gen-id)
        label {:id id
               :address cell-address
               :type label-type}]
    (assoc-in sheet [:tables table-id :labels id] label)))

(defn- look-up-table-at [sheet cell-address]
  (first
   (filter (fn [[_ {:keys [id start end]}]]
             (inside? cell-address [start end]))
           (:tables sheet))))

(defn mark-label [sheet label-type cell]
  (let [{:keys [id]} (look-up-table-at sheet cell)]
    (insert-label sheet id label-type cell)))

(defn- top-row [[r1 c1] [_ c2]]
  (map #(do [r1 %])
       (range c1 (inc c2))))

(defn- left-col [[r1 c1] [r2 _]]
  (map #(do [% c1])
       (range r1 (inc r2))))

(defn make-vtable [sheet start end]
  (let [[sheet* table] (make-table- sheet start end)]
    (reduce #(insert-label %1 (:id table) :top %2)
            sheet*
            (top-row start end))))

(defn make-htable [sheet start end]
  (let [[sheet* table] (make-table- sheet start end)]
    (reduce #(insert-label %1 (:id table) :left %2)
            sheet*
            (left-col start end))))

(defn make-xtable [sheet start end]
  (let [[sheet* table] (make-table- sheet start end)]
    (as-> sheet* sheet*
      (reduce #(insert-label %1 (:id table) :top %2)
              sheet*
              (set/difference
               (set
                (top-row start end))
               (set
                (left-col start end))))
      (reduce #(insert-label %1 (:id table) :left %2)
              sheet*
              (set/difference
               (set
                (left-col start end))
               (set
                (top-row start end)))))))

(def =>make-table [:=>
                   [:cat
                    any?
                    [:tuple int? int?]
                    [:tuple int? int?]]
                   any?])

(m/validate =>make-table make-vtable)
(m/validate =>make-table make-htable)
(m/validate =>make-table make-xtable)