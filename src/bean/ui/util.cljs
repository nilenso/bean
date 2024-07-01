(ns bean.ui.util
  (:require [clojure.string :as str]
            [bean.util :as util]))

(defn i->a [i]
  (apply
   str
   (loop [a '()
          i (inc i)]
     (let [m (mod i 26)
           n (/ i 26)]
       (if (> n 1)
         (recur (cons (char (+ 64 m)) a)
                n)
         (cons (char (+ 64 m)) a))))))

(defn rc->a1 [r c]
  (str (i->a c) (inc r)))

(defn cs [& classes]
  (->> classes
       (remove nil?)
       (map name)
       (str/join " ")))

(def map-on-matrix util/map-on-matrix)
(def addresses-matrix util/addresses-matrix)
(def map-on-matrix-addressed util/map-on-matrix-addressed)
(def offset util/offset)
(def distance util/distance)

(defn color-int->hex [color]
  (str "#" (.toString color 16)))

(defn merged-or-self [[r c] sheet]
  (or (get-in sheet [:grid r c :style :merged-with]) [r c]))

(defn merged-until-or-self [rc sheet]
  (let [[r* c*] (merged-or-self rc sheet)]
    (or (get-in sheet [:grid r* c* :style :merged-until]) rc)))

(defn area-inside? [sheet {:keys [start end]}]
  (and
   (nat-int? (first start)) (nat-int? (second start))
   (< (first end) (get-in sheet [:grid-dimensions :num-rows]))
   (< (second end) (get-in sheet [:grid-dimensions :num-cols]))))
