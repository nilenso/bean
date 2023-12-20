(ns bean.table-test
  (:require [bean.grid :refer [eval-sheet new-sheet]]
            [bean.table :as table]))

(let [grid [["header 1" "header 2" "header 3"]
            ["2" "" ""]
            ["=A1+A2" "" ""]
            ["=A3+1" "" ""]
            ["=A1+A2+A3+A4+10" "" ""]]
      evaluated-sheet (eval-sheet (new-sheet grid ""))
      ttable (table/make-table (:grid evaluated-sheet) [0 0] [3 3] :top)]
  (table/top-lookup-label "header 1" ttable (:grid evaluated-sheet)))
