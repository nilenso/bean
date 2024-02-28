(ns bean.tables
  (:require [bean.area :as area]
            [bean.util :as util]
            [clojure.set :as set]))

(defn make-table [sheet table-name area]
  (if (and (not (area/area-empty? area))
           (not (some
                 #(area/overlap? % area)
                 (vals (:tables sheet)))))
    (assoc-in sheet [:tables table-name]
              (merge area {:labels {}
                           :skip-cells #{}}))
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
    (assoc-in
     sheet
     [:tables table-name :labels (util/merged-or-self rc sheet)]
     {:dirn dirn :color color})
    sheet))

(defn add-labels [sheet table-name addresses dirn]
  (reduce #(add-label %1 table-name %2 dirn (util/random-color-hex)) sheet addresses))

(defn remove-labels [sheet table-name addresses]
  (reduce #(update-in % [:tables table-name :labels] dissoc %2) sheet addresses))

(defn get-table [sheet table-name]
  (get-in sheet [:tables table-name]))

(defn- get-label [sheet table-name rc & [dirn]]
  (let [label (get-in sheet [:tables table-name :labels rc])]
    (if dirn
      (when (= (:dirn label) dirn)
        label)
      label)))

(defn label? [sheet table-name label-name & [dirn]]
  (some
   (fn [[label label-data]]
     (and (= label-name
             (:scalar (util/get-cell (:grid sheet) label)))
          (if dirn (= (:dirn label-data) dirn) true)))
   (:labels (get-table sheet table-name))))

(defn merge-labels [sheet start addresses]
  (if-let [table-name (cell-table start sheet)]
    (let [is-label? (get-label sheet table-name start)
          other-labels? (and (not is-label?)
                             (some #(get-label sheet table-name %) addresses))
          label (or is-label? other-labels?)]
      (if label
        (-> sheet
            (remove-labels table-name addresses)
            (add-label table-name start (:dirn label) (:color label)))
        sheet))
    sheet))

(defn- last-row [[r c] sheet]
  (+ r (dec (area/cell-h sheet [r c]))))

(defn- last-col [[r c] sheet]
  (+ c (dec (area/cell-w sheet [r c]))))

(defn- left-blocking-label [sheet [r c] labels]
  (some
   (fn [[[r* c*] {:keys [dirn]}]]
     (when
      (and
       (= dirn :left)
       (= r r*)
       (= (area/cell-h sheet [r c])
          (area/cell-h sheet [r* c*]))
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
       (= (area/cell-w sheet [r c])
          (area/cell-w sheet [r* c*]))
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
        labels (:labels table)
        merged-with-labels (mapcat
                            #(get-in (util/get-cell (:grid sheet) %)
                                     [:style :merged-addresses])
                            (keys labels))]
    (when-let [{:keys [dirn]} (get labels label)]
      (as->
       (area/area->addresses
        {:start label
         :end (let [[br bc] (blocking-label sheet table-name label)]
                (case dirn
                  :top [(if br (dec br) table-end-r)
                        (min (last-col label sheet) table-end-c)]
                  :left [(min (last-row label sheet) table-end-r)
                         (if bc (dec bc) table-end-c)]))}) cells
        (apply disj cells (filter #(get labels %) cells))
        (apply disj cells merged-with-labels)))))

(defn skipped-cells [sheet table-name]
  (let [table (get-table sheet table-name)
        labels (:labels table)
        skip-labels (filter #(get-in table [:skip-cells %]) (keys labels))]
    (set/union
     (set (mapcat #(label->cells sheet table-name %) skip-labels))
     (:skip-cells table))))

(defn label-name->cells [sheet table-name label-name & [dirn]]
  ;; Excludes skip labels
  (->> (keys (:labels (get-table sheet table-name)))
       (filter #(get-label sheet table-name % dirn))
       (filter #(when (= label-name
                         (:scalar (util/get-cell (:grid sheet) %)))
                  %))
       (map #(label->cells sheet table-name %))
       (apply set/union)
       (#(apply disj % (skipped-cells sheet table-name)))))

(defn mark-skipped [sheet table-name addresses]
  (update-in sheet [:tables table-name :skip-cells] #(apply conj % (set addresses))))

(defn unmark-skipped [sheet table-name addresses]
  (let [addresses*
        (set/union
         (set addresses)
         (set (mapcat #(label->cells sheet table-name %) addresses)))]
    (update-in sheet [:tables table-name :skip-cells] #(apply disj % addresses*))))

(defn resize-table [sheet table-name area]
  (update-in sheet [:tables table-name] merge area))

(defn expand-tables [sheet [updated-r updated-c]]
  (if-let [at-end-of-table (some (fn [[table-name {:keys [start end]}]]
                                   (when (and (= updated-r (inc (first end)))
                                              (< updated-c (inc (second end)))
                                              (> updated-c (second start)))
                                     table-name)) (:tables sheet))]
    (let [[end-r end-c] (:end (get-table sheet at-end-of-table))]
      (resize-table sheet at-end-of-table {:end [(inc end-r) end-c]}))
    sheet))
