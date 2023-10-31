(ns bean.util
  (:require [cljs.math :refer [pow]]))

(def ^:private num-alphabets 26)

(defn get-cell [grid address]
  (if-let [contents (get-in grid address)]
    contents
    {:error (str "Invalid address " address)}))

(defn a1->rc [a n]
  (let [indexed-a (map vector (reverse a) (range))
        c (reduce (fn [total [alphabet i]]
                    (+ total
                       (* (pow num-alphabets i)
                          (- (.charCodeAt alphabet 0)
                             (dec (.charCodeAt "A" 0))))))
                  0
                  indexed-a)]
    [(dec n) (dec c)]))

(defn map-on-matrix [f matrix]
  (mapv #(mapv (fn [element] (f element)) %) matrix))

;; TODO: Is there a better way to return vectors instead of lists
;; for O(1) lookups later.
(defn map-on-matrix-addressed [f matrix]
  (vec (map-indexed (fn [row-idx row]
                      (vec (map-indexed
                            (fn [col-idx element]
                              (f [row-idx col-idx] element))
                            row)))
                    matrix)))

(defn reduce-on-sheet-addressed [f {:keys [grid] :as sheet}]
  (reduce (fn [sheet [addr cell]]
            (f sheet addr cell))
          sheet
          (mapcat identity (map-on-matrix-addressed vector grid))))

(defn matrix-bounds [start-ref end-ref]
  (let [[_ start-a start-n] start-ref
        [_ end-a end-n] end-ref
        start-address (a1->rc start-a (js/parseInt start-n))
        end-address (a1->rc end-a (js/parseInt end-n))]
    [start-address end-address]))

(defn addresses-matrix
  [[start-row start-col] [end-row end-col]]
  (for [r (range start-row (inc end-row))]
    (for [c (range start-col (inc end-col))]
      [r c])))

(defn is-expression? [[node-type & _]]
  (= node-type :Expression))
