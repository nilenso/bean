(ns bean.operators)

(defn bean-op-+ [left right]
  (if (and (int? left) (int? right))
    (+ left right)
    {:error "+ only works for Integers"}))

(defn bean-op-* [left right]
  (if (and (int? left) (int? right))
    (* left right)
    {:error "* only works for Integers"}))
