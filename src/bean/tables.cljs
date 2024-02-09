(ns bean.tables
  (:require [bean.grid :as grid]))

(defn make-table [sheet table-name area]
  (if (and (not (grid/area-empty? area))
           (not (some
                 #(grid/overlap? % area)
                 (vals (:tables sheet)))))
    (assoc-in sheet [:tables table-name]
              (merge area {:labels #{}}))
    sheet))

(defn add-label [sheet table-name rc dirn]
  (update-in sheet [:tables table-name :labels] conj {:address rc :dirn dirn}))

(defn cell-table [[r c] tables]
  (some
   (fn [[table-name {:keys [start end]}]]
     (let [[start-r start-c] start
           [end-r end-c] end]
       (when (and (>= r start-r)
                  (<= r end-r)
                  (>= c start-c)
                  (<= c end-c))
         table-name)))
   tables))
