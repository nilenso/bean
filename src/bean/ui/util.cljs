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

(def map-on-matrix-addressed util/map-on-matrix-addressed)

(defn color-int->hex [color]
  (str "#" (.toString color 16)))

(defn random-color-hex []
  (+ (bit-shift-left (rand-int 256) 16)
     (bit-shift-left (rand-int 256) 8)
     (rand-int 256)))
