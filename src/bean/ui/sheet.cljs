(ns bean.ui.sheet
  (:require [clojure.string :as str]
            [reagent.dom :as r]
            [cljs.core :refer [char]]))

(defn cs [& classes]
  (->> classes
       (remove nil?)
       (map name)
       (str/join " ")))

(defn cell [row col {:keys [error representation] :as cell}]
  [:div {:content-editable "true"
         :data-row row
         :data-col col
         :class (cs :bean-cell
                    (when error :cell-error))}
   representation])

(defn i->a [i]
  (loop [a '()
         i (inc i)]
    (let [m (mod i 26)
          n (/ i 26)]
      (if (> n 1)
        (recur (cons (char (+ 64 m)) a)
               n)
        (cons (char (+ 64 m)) a)))))

(defn label-cell [i & [kind]]
  [:div {:class (cs :bean-label
                    (when-not kind :bean-label-left)
                    (when (= i :bean) :bean-corner))}
   (case kind
     :alpha (i->a i)
     :bean "Bean"
     (str i))])

(defn label-row [rows]
  [:div {:class (cs :bean-label-row)}
   [label-cell :bean :bean]
        (map-indexed
         (fn [i _] ^{:key i}
           [label-cell i :alpha])
         (first rows))])

(defn row [i cells]
  [:div {:class (cs :bean-row)}
   [label-cell i]
   (map-indexed #(do ^{:key %1}
                  [cell i %1 %2])
                cells)])

(defn sheet1 [rows]
  [:div {:class "bean-sheet"}
   [label-row rows]
   (map-indexed #(do ^{:key %1}
                  [row %1 %2])
                rows)])
