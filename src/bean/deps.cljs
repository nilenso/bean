(ns bean.deps
  (:require [clojure.set :as set]
            [bean.util :as util]))

(defn ->ref-dep [dependent-address]
  "Return a representation of a :ref dependent for the given address."
  [:ref dependent-address])

(defn ast->deps [ast]
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

(defn resolve-dependents [{:keys [depgraph]} deps]
  "Iterate over the dependents and return a set of resolved dependents. :ref
   dependents are considered resolved.

   - :ref dependents are resolved by their own addresses
   - :binding dependents are resolved by the dependents of the
     binding found in the depgraph"
  (loop [deps deps
         addrs #{}]
    (if (empty? deps)
      addrs
      (let [[dt d :as dep] (first deps)]
        (case dt
          :ref (recur
                (rest deps)
                (conj addrs d))
          :binding (recur
                    (concat (rest deps) (get depgraph dep))
                    addrs))))))

(defn immediate-dependents [{:keys [depgraph]} addrs]
  "Returns a set of dependents of the given addresses."
  (->> addrs
       (map ->ref-dep)
       (map depgraph)
       (mapcat identity)
       set))

(comment
  (resolve-dependents {:depgraph {[:ref [0 0]] #{[:ref [1 1]] [:binding "ƒoo"]}
                                        [:binding "ƒoo"] #{[:ref [1 2]] [:binding "bar"]}
                                        [:binding "bar"] #{[:ref [3 3]]}}}
                            [[:ref [0 0]] [:binding "ƒoo"]]))

(defn- depgraph-add-edge [depgraph parent child]
  (assoc depgraph parent (conj (get depgraph parent #{}) child)))

(defn- depgraph-remove-edge [depgraph parent child]
  (assoc depgraph parent (disj (get depgraph parent) child)))

(defn make-depgraph [grid]
  "Iterates through each cell in the grid and their dependencies and returns
   an adjacency matrix of the dependency graph"
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