(ns bean.tables
  (:require [bean.grid :as grid]
            [bean.util :as util]))

(defn make-table [sheet table-name area]
  (if (and (not (grid/area-empty? area))
           (not (some
                 #(grid/overlap? % area)
                 (vals (:tables sheet)))))
    (assoc-in sheet [:tables table-name]
              (merge area {:labels {}}))
    sheet))

(defn cell-table [[r c] sheet]
  (some
   (fn [[table-name {:keys [start end]}]]
     (let [[start-r start-c] start
           [end-r end-c] end]
       (when (and (>= r start-r)
                  (<= r end-r)
                  (>= c start-c)
                  (<= c end-c))
         table-name)))
   (:tables sheet)))

(defn add-label [sheet table-name rc dirn & [color]]
  (if (= (cell-table rc sheet) table-name)
    (assoc-in sheet [:tables table-name :labels rc] {:dirn dirn :color color})
    sheet))

(defn add-labels [sheet table-name addresses dirn]
  (reduce #(add-label %1 table-name %2 dirn (util/random-color-hex)) sheet addresses))

(defn remove-labels [sheet table-name addresses]
  (reduce #(update-in % [:tables table-name :labels] dissoc %2) sheet addresses))

(defn get-table [sheet table-name]
  (get-in sheet [:tables table-name]))

(defn- last-row [[r c] sheet]
  (+ r (dec (grid/cell-h sheet [r c]))))

(defn- last-col [[r c] sheet]
  (+ c (dec (grid/cell-w sheet [r c]))))

(defn- left-blocking-label [sheet [r c] labels]
  (some
   (fn [[[r* c*] {:keys [dirn]}]]
     (when
      (and
       (= dirn :left)
       (= r r*)
       (= (grid/cell-h sheet [r c])
          (grid/cell-h sheet [r* c*]))
       (> c* (last-col [r c] sheet)))
       [r* c*]))
   (sort-by (fn [[[_ c] _]] c) labels)))

(defn- top-blocking-label [sheet [r c] labels]
  (some
   (fn [[[r* c*] {:keys [dirn]}]]
     (when
      (and
       (= dirn :top)
       (= c c*)
       (= (grid/cell-w sheet [r c])
          (grid/cell-w sheet [r* c*]))
       (> r* (last-row [r c] sheet)))
       [r* c*]))
   (sort-by (fn [[[r _] _]] r) labels)))

(defn blocking-label [sheet table-name label]
  (let [{:keys [labels] :as table} (get-table sheet table-name)
        {:keys [dirn]} (get-in table [:labels label])]
    (case dirn
      :top (top-blocking-label sheet label labels)
      :left (left-blocking-label sheet label labels))))

(defn label->cells [sheet table-name label]
  (let [{:keys [end] :as table} (get-table sheet table-name)
        [table-end-r table-end-c] end
        {:keys [dirn]} (get-in table [:labels label])]
    (as->
     (grid/area->addresses
      {:start label
       :end (let [[br bc] (blocking-label sheet table-name label)]
              (case dirn
                :top [(if br (dec br) table-end-r)
                      (min (last-col label sheet) table-end-c)]
                :left [(min (last-row label sheet) table-end-r)
                       (if bc (dec bc) table-end-c)]))}) cells
      (apply disj cells (filter #(get (:labels table) %) cells))
      (apply disj cells
             (get-in (util/get-cell (:grid sheet) label)
                     [:style :merged-addresses])))))
