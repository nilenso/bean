(ns bean.deps
  (:require [clojure.set :as set]
            [bean.util :as util]))

(defn ->ref-dep [dep]
  [:ref dep])

(defn- ast->deps [ast]
  (let [[node-type & [arg :as args]] ast]
    (case node-type
      :CellContents (ast->deps arg)
      :FunctionInvocation (apply set/union
                                 (map ast->deps args))
      :CellRef (let [[_ a n] ast]
                 #{(->ref-dep (util/a1->rc a (js/parseInt n)))})
      :MatrixRef (->> (apply util/matrix-bounds args)
                      (apply util/addresses-matrix)
                      (mapcat identity)
                      (map ->ref-dep)
                      set)
      :Expression (if (util/is-expression? arg)
                    (let [[left _ right] args]
                      (set/union
                       (ast->deps left)
                       (ast->deps right)))
                    (ast->deps arg))
      #{})))

(defn- depgraph-add-edge [depgraph parent child]
  (assoc depgraph parent (conj (get depgraph parent #{}) child)))

(defn- depgraph-remove-edge [depgraph parent child]
  (assoc depgraph parent (disj (get depgraph parent) child)))

(defn make-depgraph [grid]
  (->> grid
       (util/map-on-matrix-addressed
        #(for [dependency (ast->deps (:ast %2))]
           {:parent dependency :child (->ref-dep %1)}))
       flatten
       (reduce #(depgraph-add-edge %1 (:parent %2) (:child %2)) {})))

(defn update-depgraph [depgraph address old-cell new-cell]
  (let [old-dependencies (ast->deps (:ast old-cell))
        new-dependencies (ast->deps (:ast new-cell))]
    (as-> depgraph g
      (reduce #(depgraph-remove-edge %1 %2 (->ref-dep address)) g old-dependencies)
      (reduce #(depgraph-add-edge %1 %2 (->ref-dep address)) g new-dependencies))))
