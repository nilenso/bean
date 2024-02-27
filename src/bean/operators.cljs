(ns bean.operators
  (:require [bean.errors :as errors]))

(defn bean-op-+ [left right]
  (if (and (int? left) (int? right))
    (+ left right)
    (errors/type-mismatch-+-op)))

(defn bean-op-< [left right]
  (if (and (int? left) (int? right))
    (< left right)
    (errors/type-mismatch-<-op)))

(defn bean-op-> [left right]
  (if (and (int? left) (int? right))
    (> left right)
    (errors/type-mismatch->-op)))

(defn bean-op-= [left right]
  (if (and (int? left) (int? right))
    (= left right)
    (errors/type-mismatch-=-op)))

(defn bean-op-* [left right]
  (if (and (int? left) (int? right))
    (* left right)
    (errors/type-mismatch-*-op)))
