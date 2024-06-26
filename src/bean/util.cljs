(ns bean.util
  (:require [cljs.math :refer [pow]]))

(def ^:private num-alphabets 26)

(defn get-cell [grid address]
  (if-let [contents (get-in grid address)]
    contents
    {:error (str "Invalid address " address)}))

(defn is-expression? [[node-type & _]]
  (= node-type :Expression))

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
  [[start-r start-c] [end-r end-c]]
  (for [r (range start-r (inc end-r))]
    (for [c (range start-c (inc end-c))]
      [r c])))

(defn random-color-hex
  ([]
   (random-color-hex (rand-int 1000000)))
  ([seed]
   (let [hash (hash seed)
         r (mod (bit-shift-right hash 16) 256)
         g (mod (bit-shift-right hash 8) 256)
         b (mod hash 256)]
     (+ (bit-shift-left r 16)
        (bit-shift-left g 8)
        b))))

(defn merged-or-self [[r c] sheet]
  (or (get-in sheet [:grid r c :style :merged-with]) [r c]))

(defn offset [[start-r start-c] [offset-rows offset-cols]]
  [(+ start-r offset-rows) (+ start-c offset-cols)])

(defn distance [[r1 c1] [r2 c2]]
  [(- r2 r1) (- c2 c1)])
