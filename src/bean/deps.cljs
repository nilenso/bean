(ns bean.deps
  (:require [clojure.set :as set]))

(defn ->ref-dep [dep]
  [:ref dep])

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