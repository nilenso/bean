(ns bean.functions
  (:require [bean.area :as area]
            [bean.errors :as errors]
            [bean.interpreter :as interpreter]
            [bean.frames :as frames]
            [bean.util :as util]
            [clojure.set :as set]
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

(defn- new-frame-result [sheet frame-result new-selection]
  (let [new-selection (set new-selection)]
    {:matrix (util/map-on-matrix
              #(if (get new-selection %)
                 (util/get-cell (:grid sheet) %)
                 {:scalar "" :representation ""})
              (area/addresses->address-matrix new-selection))
     :frame (merge frame-result
                   {:selection new-selection})}))

(defn bean-row [sheet args]
  (let [frame-result (:frame (first args))
        selection (:selection frame-result)
        frame (frames/get-frame sheet (:name frame-result))
        [start-r start-c] (:start frame)
        [end-r end-c] (:end frame)
        cols (range start-c (inc end-c))
        labels (set (keys (:labels frame)))]
    (as-> (for [col cols]
            (for [[r _] selection]
              [r col])) rcs
      (mapcat identity rcs)
      (set rcs)
      (apply disj rcs labels)
      (new-frame-result sheet frame-result rcs))))

(defn bean-col [sheet args]
  (let [frame-result (:frame (first args))
        selection (:selection frame-result)
        frame (frames/get-frame sheet (:name frame-result))
        [start-r start-c] (:start frame)
        [end-r end-c] (:end frame)
        rows (range start-r (inc end-r))
        labels (set (keys (:labels frame)))]
    (as-> (for [row rows]
            (for [[_ c] selection]
              [row c])) rcs
      (mapcat identity rcs)
      (set rcs)
      (apply disj rcs labels)
      (new-frame-result sheet frame-result rcs))))

(defn bean-reduce [sheet args]
  (let [frame-result (:frame (first args))
        f (second args)
        f* #(interpreter/apply-f-args sheet f [%1 %2])
        val* (first (drop 2 args))
        col* (->> (:selection frame-result)
                  sort
                  (map #(util/get-cell (:grid sheet) %))
                  (remove #(clojure.string/blank? (:scalar %))))]
    (if val*
      (reduce f* val* col*)
      (reduce f* col*))))

;; This doesn't work for matrices right now
;; It should: eval-matrix should perhaps return a :selection also
(defn bean-filter [sheet args]
  (let [frame-result (:frame (first args))
        f (second args)
        selection (:selection frame-result)]
    (->> selection
         (filter
          #(:scalar
            (interpreter/apply-f-args sheet f
             [(util/get-cell (:grid sheet) %)])))
         (new-frame-result sheet frame-result))))

(defn- bean-get* [sheet args asts & [dirn]]
  (let [frame-result (:frame (first args))
        label (:scalar (second args))
        existing-selection (:selection frame-result)
        vget-cells (frames/label-name->cells
                    sheet
                    (:name frame-result) label dirn)
        new-selection (set/intersection
                       vget-cells
                       existing-selection)]
    (if (frames/label? sheet (:name frame-result) label dirn)
      (new-frame-result sheet frame-result new-selection)
      (errors/label-not-found
       (:scalar (interpreter/eval-ast (second asts) sheet))))))

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
           :selection (area/area->addresses
                       (select-keys frame [:start :end]))
           :selection-dirn nil}})

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
