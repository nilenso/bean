(ns bean.ui.util
  (:require [clojure.string :as str]
            [bean.util :as util]))

(defn px [int]
  (str int "px"))

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

(defn top-left [[r1 c1] [r2 c2]]
  [(min r1 r2) (min c1 c2)])

(defn bottom-right [[r1 c1] [r2 c2]]
  [(max r1 r2) (max c1 c2)])

(def map-on-matrix-addressed util/map-on-matrix-addressed)

(def addresses-matrix util/addresses-matrix)

(defn selection->address-matrix [selection]
  (let [{:keys [start end]} selection]
    (addresses-matrix
     (top-left start end)
     (bottom-right start end))))

(defn selection->addresses [selection]
  (mapcat identity (selection->address-matrix selection)))

(defn color-int->hex [color]
  (str "#" (.toString color 16)))
