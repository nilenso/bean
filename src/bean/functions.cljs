(ns bean.functions
  (:require [bean.area :as area]
            [bean.errors :as errors]
            [bean.interpreter :as interpreter]
            [bean.tables :as tables]
            [bean.util :as util]
            [clojure.set :as set]
            [clojure.string]))

(defn cell-ref? [[_ ast]]
  (let [[expression-type] ast]
    (= expression-type :CellRef)))

;; Functions don't work for matrices,
;; they need the thing as apply-op does
(defn bean-concat [_sheet args]
  (interpreter/apply-results
   (fn [& arg]
     (reduce str "" arg))
   args))

(defn- new-table-result [sheet table-result new-selection]
  (let [new-selection (set new-selection)]
    {:matrix (util/map-on-matrix
              #(if (get new-selection %)
                 (util/get-cell (:grid sheet) %)
                 {:scalar "" :representation ""})
              (area/addresses->address-matrix new-selection))
     :table (merge table-result
                   {:selection new-selection})}))

(defn bean-row [sheet args]
  (let [table-result (:table (first args))
        selection (:selection table-result)
        table (tables/get-table sheet (:name table-result))
        [start-r start-c] (:start table)
        [end-r end-c] (:end table)
        cols (range start-c (inc end-c))
        labels (set (keys (:labels table)))]
    (as-> (for [col cols]
            (for [[r _] selection]
              [r col])) rcs
      (mapcat identity rcs)
      (set rcs)
      (apply disj rcs labels)
      (new-table-result sheet table-result rcs))))

(defn bean-col [sheet args]
  (let [table-result (:table (first args))
        selection (:selection table-result)
        table (tables/get-table sheet (:name table-result))
        [start-r start-c] (:start table)
        [end-r end-c] (:end table)
        rows (range start-r (inc end-r))
        labels (set (keys (:labels table)))]
    (as-> (for [row rows]
            (for [[_ c] selection]
              [row c])) rcs
      (mapcat identity rcs)
      (set rcs)
      (apply disj rcs labels)
      (new-table-result sheet table-result rcs))))

(defn bean-reduce [sheet args]
  (let [table-result (:table (first args))
        f (second args)
        f* #(interpreter/apply-f-args sheet f [%1 %2])
        val* (first (drop 2 args))
        col* (->> (:selection table-result)
                  sort
                  (map #(util/get-cell (:grid sheet) %))
                  (remove #(clojure.string/blank? (:scalar %))))]
    (if val*
      (reduce f* val* col*)
      (reduce f* col*))))

;; This doesn't work for matrices right now
;; It should: eval-matrix should perhaps return a :selection also
(defn bean-filter [sheet args]
  (let [table-result (:table (first args))
        f (second args)
        selection (:selection table-result)]
    (->> selection
         (filter
          #(:scalar
            (interpreter/apply-f-args sheet f
             [(util/get-cell (:grid sheet) %)])))
         (new-table-result sheet table-result))))

(defn- bean-get* [sheet args asts & [dirn]]
  (let [table-result (:table (first args))
        label (:scalar (second args))
        existing-selection (:selection table-result)
        vget-cells (tables/label-name->cells
                    sheet
                    (:name table-result) label dirn)
        new-selection (set/intersection
                       vget-cells
                       existing-selection)]
    (if (tables/label? sheet (:name table-result) label dirn)
      (new-table-result sheet table-result new-selection)
      (errors/label-not-found
       (:scalar (interpreter/eval-ast (second asts) sheet))))))

(defn bean-get [sheet args asts]
  (bean-get* sheet args asts))

(defn bean-vget [sheet args asts]
  (bean-get* sheet args asts :top))

(defn bean-hget [sheet args asts]
  (bean-get* sheet args asts :left))

(defn bean-table [sheet args asts]
  (if (cell-ref? (first asts))
    (let [[_ [_ a n]] (first asts)
          address (util/a1->rc a (js/parseInt n))
          table-name (tables/cell-table address sheet)
          table (tables/get-table sheet table-name)]
      (if table-name
        {:matrix (interpreter/eval-matrix (:start table)
                                          (:end table)
                                          (:grid sheet))
         :table {:name table-name
                 :selection (area/area->addresses
                             (select-keys table [:start :end]))
                 :selection-dirn nil}}
        (errors/undefined-table-at (str a n))))
    (errors/invalid-table-args
     (str (:scalar (first args))))))

(defn bean-error [_sheet args]
  (let [str-err (str (:error (first args)))]
    {:scalar str-err
     :representation str-err}))
