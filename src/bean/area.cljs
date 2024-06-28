(ns bean.area
  (:require [bean.util :as util]))

(defn- top-left [addresses]
  [(apply min (map first addresses))
   (apply min (map second addresses))])

(defn- bottom-right [addresses]
  [(apply max (map first addresses))
   (apply max (map second addresses))])

(defn bounds->area [start end]
  {:start (top-left [start end])
   :end (bottom-right [start end])})

(defn area->address-matrix [{:keys [start end]}]
  (util/addresses-matrix start end))

(defn area->addresses [area]
  (set (mapcat identity (area->address-matrix area))))

(defn addresses->area [addresses]
  {:start (top-left addresses)
   :end (bottom-right addresses)})

(defn addresses->address-matrix [addresses]
  (area->address-matrix (addresses->area addresses)))

(defn area-empty? [{:keys [start end]}]
  (= start end))

(defn overlap? [area-a area-b]
  (let [{[a-r1 a-c1] :start [a-r2 a-c2] :end} area-a
        {[b-r1 b-c1] :start [b-r2 b-c2] :end} area-b]
    (not
     (or (< a-r2 b-r1)
         (> a-r1 b-r2)
         (< a-c2 b-c1)
         (> a-c1 b-c2)))))

(defn cell-h [sheet [r c]]
  (let [cell (util/get-cell (:grid sheet) [r c])
        [end-r _] (get-in cell [:style :merged-until])]
    (if end-r (inc (- end-r r)) 1)))

(defn cell-w [sheet [r c]]
  (let [cell (util/get-cell (:grid sheet) [r c])
        [_ end-c] (get-in cell [:style :merged-until])]
    (if end-c (inc (- end-c c)) 1)))
