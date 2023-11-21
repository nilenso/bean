(ns bean.deps-test
  (:require [bean.grid :refer [new-sheet eval-sheet]]
            [bean.deps :refer [make-depgraph update-depgraph]]
            [clojure.test :refer [deftest testing is]]))

(deftest depgraph-update-test
  (testing "Adds an edge to the depgraph to a node that doesn't already exist"
    (is
     (= {[:cell [0 0]] #{[:cell [0 1]]}}
        (let [grid (:grid (eval-sheet (new-sheet [["1" "2"]] "")))]
          (update-depgraph (make-depgraph grid)
                           [:cell [0 1]]
                           (get-in grid [0 1])
                           {:content "=A1"
                            :ast [:CellContents [:Expression [:CellRef "A" "1"]]]
                            :scalar 1
                            :representation "1"})))))

  (testing "Adds an edge to the depgraph to a node that already has a dependency"
    (is
     (= {[:cell [0 0]] #{[:cell [0 1]] [:cell [0 2]]}}
        (let [grid (:grid (eval-sheet (new-sheet [["1" "2" "=A1"]] "")))]
          (update-depgraph (make-depgraph grid)
                           [:cell [0 1]]
                           (get-in grid [0 1])
                           {:content "=A1"
                            :ast [:CellContents [:Expression [:CellRef "A" "1"]]]
                            :scalar 1
                            :representation "1"})))))

  (testing "Removes a node from the depgraph if it has no edges after update"
    (is
     (= {}
        (let [grid (:grid (eval-sheet (new-sheet [["1" "=A1"]] "")))]
          (update-depgraph (make-depgraph grid)
                           [:cell [0 1]]
                           (get-in grid [0 1])
                           {:content "2"
                            :ast [:CellContents [:Integer "2"]]
                            :scalar 2
                            :representation "2"}))))))