(ns bean.functions
  (:require [bean.area :as area]
            [bean.errors :as errors]
            [bean.interpreter :as interpreter]
            [bean.frames :as frames]
            [bean.util :as util]
            [clojure.string]))

(defn cell-ref? [[_ ast]]
  (let [[expression-type] ast]
    (= expression-type :CellRef)))

(defn quoted-string? [[_ ast]]
  (let [[expression-type] ast]
    (= expression-type :QuotedString)))

;; Functions don't work for matrices,
;; they need the thing as apply-op does
(defn bean-concat [_sheet args]
  (interpreter/apply-results
   (fn [& arg]
     (reduce str "" arg))
   args))

(defn- address-matrix->cells-matrix [sheet matrix]
  (util/map-on-matrix
   #(if %
      (util/get-cell (:grid sheet) %)
      {:scalar "" :representation ""})
   matrix))

(defn- remove-nil-columns [matrix]
  (let [columns (apply map vector matrix)
        non-nil-columns (remove #(every? nil? %) columns)]
    (if (empty? non-nil-columns)
      []
      (apply map vector non-nil-columns))))

(defn- remove-nil-rows [matrix]
  (remove #(every? nil? %) matrix))

(defn minimum-matrix [matrix]
  (if (zero? (count (first matrix)))
    [[nil]]
    matrix))

(defn bean-transpose [sheet args]
  (if-not (:error (first args))
    (let [frame-result (:frame (first args))
          new-selection (apply mapv vector (:selection frame-result))]
      {:matrix (address-matrix->cells-matrix sheet (minimum-matrix new-selection))
       :frame (merge frame-result {:selection new-selection})})
    (first args)))

(defn bean-row [sheet args]
  (if-not (:error (first args))
    (let [frame-result (:frame (first args))
          selection (:selection frame-result)
          frame (frames/get-frame sheet (:name frame-result))
          [start-r start-c] (:start frame)
          [end-r end-c] (:end frame)
          cols (range start-c (inc end-c))
          rows (map first (mapcat identity selection))
          new-selection (for [r rows]
                          (for [col cols]
                            [r col]))]
      {:matrix (address-matrix->cells-matrix sheet (minimum-matrix new-selection))
       :frame (merge frame-result {:selection new-selection})})
    (first args)))

(defn bean-col [sheet args]
  (if-not (:error (first args))
    (let [frame-result (:frame (first args))
          selection (:selection frame-result)
          frame (frames/get-frame sheet (:name frame-result))
          [start-r start-c] (:start frame)
          [end-r end-c] (:end frame)
          cols (map second (mapcat identity selection))
          rows (range start-r (inc end-r))
          new-selection (for [r rows]
                          (for [col cols]
                            [r col]))]
      {:matrix (address-matrix->cells-matrix sheet (minimum-matrix new-selection))
       :frame (merge frame-result {:selection new-selection})})
    (first args)))

(defn bean-reduce [sheet args]
  (let [frame-result (:frame (first args))
        f (second args)
        f* #(interpreter/apply-f-args sheet f [%1 %2])
        val* (first (drop 2 args))
        col* (->> (:selection frame-result)
                  (mapcat identity)
                  sort
                  (map #(util/get-cell (:grid sheet) %))
                  (remove #(clojure.string/blank? (:scalar %))))]
    (if val*
      (reduce f* val* col*)
      (reduce f* col*))))

;; These don't work for matrices right now
;; It should: eval-matrix should perhaps return a :selection also
(defn bean-filter [sheet args]
  (if (and (not (:error (first args)))
           (second args))
    (let [frame-result (:frame (first args))
          f (second args)
          new-selection (->> (:selection frame-result)
                             (util/map-on-matrix
                              #(when (:scalar
                                      (interpreter/apply-f-args
                                       sheet f [(util/get-cell (:grid sheet) %)]))
                                 %))
                             remove-nil-columns
                             remove-nil-rows)]
      {:matrix (address-matrix->cells-matrix sheet (minimum-matrix new-selection))
       :frame (merge frame-result {:selection new-selection})})
    (first args)))

(defn bean-match [sheet args]
  (if-not (:error (first args))
    (let [from-frame (:frame (first args))
          to-frame (:frame (second args))]
      (when (and from-frame to-frame)
        (let [first-match
              (reduce
               #(if (get %1 %2)
                  %1
                  (assoc %1 (:representation (util/get-cell (:grid sheet) %2)) %2))
               {}
               (mapcat identity (:selection to-frame)))

              new-selection
              (->> (util/map-on-matrix
                    #(let [value (:representation (util/get-cell (:grid sheet) %))]
                       (get first-match value))
                    (:selection from-frame))
                   remove-nil-columns
                   remove-nil-rows)]
          {:matrix (address-matrix->cells-matrix sheet (minimum-matrix new-selection))
           :frame {:selection new-selection
                   :name (:name to-frame)}})))
    (first args)))

(defn- bean-get* [sheet args asts & [dirn]]
  (if-not (:error (first args))
    (let [frame-result (:frame (first args))
          label (:scalar (second args))]
      (if (frames/label? sheet (:name frame-result) label dirn)
        (let [label-cells (frames/label-name->cells
                           sheet
                           (:name frame-result) label dirn)
              new-selection (->> (:selection frame-result)
                                 (util/map-on-matrix
                                  #(when (or (contains? (:cells label-cells) %)
                                             (and (contains? (:skips frame-result) %)
                                                  (contains? (:skips label-cells) %))) %))
                                 remove-nil-columns
                                 remove-nil-rows
                                ;;  Hack for null references cells
                                ;;  these come from top left labels
                                 (util/map-on-matrix
                                  #(or % [79 15])))]
          {:matrix (address-matrix->cells-matrix sheet (minimum-matrix new-selection))
           :frame (merge frame-result {:selection new-selection})})
        (errors/label-not-found
         (:scalar (interpreter/eval-ast (second asts) sheet)))))
    (first args)))

(defn bean-get [sheet args asts]
  (bean-get* sheet args asts))

(defn bean-vget [sheet args asts]
  (bean-get* sheet args asts :top))

(defn bean-hget [sheet args asts]
  (bean-get* sheet args asts :left))

(defn bean-frame* [sheet frame frame-name]
  {:matrix (interpreter/eval-matrix (:start frame)
                                    (:end frame)
                                    (:grid sheet))
   :frame {:name frame-name
           :selection (area/area->address-matrix frame)}})

(defn bean-frame [sheet args asts]
  (cond
    (cell-ref? (first asts))
    (let [[_ [_ a n]] (first asts)
          address (util/a1->rc a (js/parseInt n))
          frame-name (frames/cell-frame address sheet)
          frame (frames/get-frame sheet frame-name)]
      (if frame-name
        (bean-frame* sheet frame frame-name)
        (errors/undefined-frame-at (str a n))))

    (quoted-string? (first asts))
    (let [[_ [_ frame-name]] (first asts)
          frame (frames/get-frame sheet frame-name)]
      (if frame
        (bean-frame* sheet frame frame-name)
        (errors/undefined-frame-at frame-name)))

    :else (errors/invalid-frame-args
           (str (:scalar (first args))))))

(defn bean-error [_sheet args]
  (let [str-err (str (:error (first args)))]
    {:scalar str-err
     :representation str-err}))
