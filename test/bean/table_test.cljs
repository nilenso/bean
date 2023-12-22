(ns bean.table-test
  (:require [clojure.test :refer [deftest is testing are]]
            [bean.grid :refer [eval-sheet new-sheet]]
            [bean.table :as table]))

(let [grid [["header 1" "header 2" "header 3"]
            ["2" "" ""]
            ["=A1+A2" "" ""]
            ["=A3+1" "" ""]
            ["=A1+A2+A3+A4+10" "" ""]]
      evaluated-sheet (eval-sheet (new-sheet grid ""))
      ttable (table/make-table (:grid evaluated-sheet) [0 0] [3 3] :top)]
  (table/top-lookup-label "header 1" ttable (:grid evaluated-sheet)))

(deftest test-is-top-left-of
  (testing "Returns true only when first address is to the top left of the second address"
    (are [start stop result] (= result (table/is-top-left-of start stop))
      [1 1] [4 4] true
      [4 4] [1 1] false
      [1 4] [4 1] false
      [4 4] [1 4] false
      [1 1] [4 1] true
      [4 4] [1 1] false
      [1 4] [1 4] true
      [4 1] [1 4] false)))

(deftest no-tables-intersect
  (testing "Returns true when none of the tables contain the given cell"
    (let [tables [{:start [3 3]
                   :end [5 5]}
                  {:start [3 9]
                   :end [5 11]}
                  {:start [7 7]
                   :end [7 7]}]]
      (are [start end result] (= result (table/no-tables-intersect tables start end))
        [1 1] [3 2] true
        [1 1] [2 3] true
        [5 6] [6 6] true
        [6 5] [6 6] true
        [1 1] [3 3] false
        [7 7] [7 7] false))))