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
  (util/rc->a1 r c))

(defn a1->rc [a n]
  (util/a1->rc a n))

(defn cs [& classes]
  (->> classes
       (remove nil?)
       (map name)
       (str/join " ")))