(ns bean.operators
  (:require [bean.errors :as errors]))

;; This is a hack to work around JS's float weirdnesses.
(defn format-number
  [num]
  (if (.isInteger js/Number num) num (js/Number (.toFixed num 4))))

(defn bean-op-+ [left right]
  (if (and (number? left) (number? right))
    (+ left right)
    (errors/type-mismatch-op "+")))

(defn bean-op-minus [left right]
  (if (and (number? left) (number? right))
    (format-number (- left right))
    (errors/type-mismatch-op "-")))

(defn bean-op-div [left right]
  (if (and (number? left) (number? right))
    (if (zero? right)
      (errors/divide-by-zero)
      (format-number (/ left right)))
    (errors/type-mismatch-op "/")))

(defn bean-op-< [left right]
  (if (and (number? left) (number? right))
    (< left right)
    (errors/type-mismatch-op "<")))

(defn bean-op-> [left right]
  (if (and (number? left) (number? right))
    (> left right)
    (errors/type-mismatch-op ">")))

(defn bean-op-= [left right]
  (if (and (or (string? left)
               (number? left))
           (or (string? right)
               (number? right)))
    (= left right)
    (errors/type-mismatch-op "=")))

(defn bean-op-* [left right]
  (if (and (number? left) (number? right))
    (* left right)
    (errors/type-mismatch-op "*")))
