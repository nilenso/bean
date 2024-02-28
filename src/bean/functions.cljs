(ns bean.functions
  (:require [bean.area :as area]
            [bean.errors :as errors]
            [bean.interpreter :as interpreter]
            [bean.tables :as tables]
            [bean.util :as util]
            [clojure.set :as set]))

(defn cell-ref? [[_ ast]]
  (let [[expression-type] ast]
    (= expression-type :CellRef)))

(defn bean-concat [sheet asts]
  (interpreter/apply-results
   (fn [& args]
     (reduce str "" args))
   (interpreter/eval-asts sheet asts)))

(defn- make-table-result [sheet table-result new-selection]
  (let [new-selection (set new-selection)]
    {:matrix (util/map-on-matrix
              #(if (get new-selection %)
                 (util/get-cell (:grid sheet) %)
                 {:scalar "" :representation ""})
              (area/addresses->address-matrix new-selection))
     :table (merge table-result
                   {:selection new-selection})}))

(defn bean-row [sheet asts]
  (let [ast-results (interpreter/eval-asts sheet asts)
        error (interpreter/first-error ast-results)]
    (if-not error
      (let [table-result (:table (first ast-results))
<<<<<<< ours
<<<<<<< Updated upstream
            label (:scalar (second ast-results))
            existing-selection (:selection table-result)
            vget-cells (tables/label-name->cells
                        sheet
                        (:name table-result) label dirn)
            new-selection (set/intersection
                           vget-cells
                           existing-selection)]
        (if (tables/label? sheet (:name table-result) label dirn)
          {:matrix (util/map-on-matrix
                    #(if (get new-selection %)
                       (util/get-cell (:grid sheet) %)
                       {:scalar "" :representation ""})
                    (area/addresses->address-matrix new-selection))
           :table (merge table-result
                         {:selection new-selection
                          :selection-dirn :top})}
          (errors/label-not-found
           (:scalar (interpreter/eval-ast (second asts) sheet)))))
=======
=======
>>>>>>> theirs
            selection (:selection table-result)
            table (tables/get-table sheet (:name table-result))
            [start-r start-c] (:start table)
            [end-r end-c] (:end table)]
        (make-table-result
         sheet
         table-result
         (apply set/union
                (map
                 (fn [[r c]]
                   (set
                    (map
                     #(do [r %])
                     (range start-c (inc end-c)))))
                 selection))))
<<<<<<< ours
>>>>>>> Stashed changes
=======
>>>>>>> theirs
      error)))

(defn bean-reduce [sheet asts]
  (let [ast-results (interpreter/eval-asts sheet asts)
        error (interpreter/first-error ast-results)]
    (if-not error
      (let [table-result (:table (first ast-results))
            f (:scalar (second ast-results))
            selection (:selection table-result)]
        (if (empty? selection)
          {:matrix [[{:scalar "" :representation ""}]]}
          (apply
           reduce
           (remove
            nil?
            [
            ;;  handle blank cells well
             #(interpreter/apply-user-f sheet f [%1 %2])
             (first (next (next ast-results)))
             (map #(util/get-cell (:grid sheet) %) selection)]))))
      error)))

;; This doesn't work for matrices right now
;; It should: eval-matrix should perhaps return a :selection also
(defn bean-filter [sheet asts]
  (let [ast-results (interpreter/eval-asts sheet asts)
        error (interpreter/first-error ast-results)]
    (if-not error
      (let [table-result (:table (first ast-results))
            f (:scalar (second ast-results))
            selection (:selection table-result)]
        (make-table-result
         sheet table-result
         (filter
          #(:scalar
            (interpreter/apply-user-f sheet f
                                      [(util/get-cell (:grid sheet) %)]))
          selection)))
      error)))

  (defn bean-get* [sheet asts & [dirn]]
    (let [ast-results (interpreter/eval-asts sheet asts)
          error (interpreter/first-error ast-results)]
      (if-not error
        (let [table-result (:table (first ast-results))
              label (:scalar (second ast-results))
              existing-selection (:selection table-result)
              vget-cells (tables/label-name->cells
                          sheet
                          (:name table-result) label dirn)
              new-selection (set/intersection
                             vget-cells
                             existing-selection)]
          (if (tables/label? sheet (:name table-result) label dirn)
            (make-table-result sheet table-result new-selection)
            (errors/label-not-found
             (:scalar (interpreter/eval-ast (second asts) sheet)))))
        error)))

  (defn bean-get [sheet asts]
    (bean-get* sheet asts))

  (defn bean-vget [sheet asts]
    (bean-get* sheet asts :top))

  (defn bean-hget [sheet asts]
    (bean-get* sheet asts :left))

  (defn bean-table [sheet asts]
    (let [ast-results (interpreter/eval-asts sheet asts)
          error (interpreter/first-error ast-results)]
      (if-not error
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
           (str (:scalar (first ast-results)))))
        error)))

  (defn bean-error [sheet asts]
    (let [str-err (str (:error (interpreter/eval-ast (first asts) sheet)))]
      {:scalar str-err
       :representation str-err}))
