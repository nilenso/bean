(ns bean.grid-test
  (:require [bean.grid :refer [parse-grid
                               eval-sheet
                               eval-code
                               eval-cell
                               new-sheet] :as grid]
            [bean.deps :refer [make-depgraph]]
            [bean.util :as util]
            [clojure.test :refer [deftest testing is]]
            [bean.area :as area]
            [bean.tables :as tables]))

(deftest evaluator-test
  (testing "Basic evaluation"
    (let [grid [["1" "" ""]
                ["2" "" ""]
                ["=A1+A2" "" ""]
                ["=A3+1" "" ""]
                ["=A1+A2+A3+A4+10" "" ""]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))]
      (is (= (util/map-on-matrix
              #(select-keys % [:scalar :content :error :representation :style])
              (:grid evaluated-sheet))
             [[{:content "1" :scalar 1 :representation "1"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]
              [{:content "2" :scalar 2 :representation "2"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]
              [{:content "=A1+A2" :scalar 3 :representation "3"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]
              [{:content "=A3+1" :scalar 4 :representation "4"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]
              [{:content "=A1+A2+A3+A4+10" :scalar 20 :representation "20"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]]))
      (is (= (:depgraph evaluated-sheet)
             {[:cell [0 0]] #{[:cell [2 0]] [:cell [4 0]]}
              [:cell [1 0]] #{[:cell [2 0]] [:cell [4 0]]}
              [:cell [2 0]] #{[:cell [3 0]] [:cell [4 0]]}
              [:cell [3 0]] #{[:cell [4 0]]}}))))

  (testing "Returns errors"
    (let [grid [["=1" "" ""]
                ["ABC" "=A1000" ""]
                ["=A1+A2" "=B2" ""]
                ["=A3+1" "" ""]]]
      (is (= (util/map-on-matrix
              #(select-keys % [:scalar :content :error :representation])
              (:grid (eval-sheet (new-sheet grid ""))))
             [[{:content "=1" :scalar 1 :representation "1"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]
              [{:content "ABC" :scalar "ABC" :representation "ABC"}
               {:content "=A1000" :scalar nil :error "Invalid address [999 0]" :representation "Invalid address [999 0]"}
               {:content "" :scalar "" :representation ""}]
              [{:content "=A1+A2" :scalar nil :error "+ only works for Integers" :representation "+ only works for Integers"}
               {:content "=B2" :scalar nil :error "Invalid address [999 0]" :representation "Invalid address [999 0]"}
               {:content "" :scalar "" :representation ""}]
              [{:content "=A3+1" :scalar nil :error "+ only works for Integers" :representation "+ only works for Integers"}
               {:content "" :scalar "" :representation ""}
               {:content "" :scalar "" :representation ""}]]))))

  (testing "Errors are not operated upon further"
    (let [grid [["=A1000+1" "=A1+100" ""]]]
      (is (= (util/map-on-matrix
              #(select-keys % [:error])
              (:grid (eval-sheet (new-sheet grid ""))))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {}]]))))

  (testing "If a cell has an error, all dependent cells become errors"
    (let [grid [["=A1000" "=A1" "=B1"]]]
      (is (= (util/map-on-matrix
              #(select-keys % [:error])
              (:grid (eval-sheet (new-sheet grid ""))))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}]]))))

  (testing "Matrix evaluation"
    (let [grid [["1" ""]
                ["2" "=A1:A2"]
                ["" ""]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))
          matrix (get-in evaluated-sheet [:grid 1 1 :matrix])]
      (is (= matrix [[{:content "1"
                       :ast [:CellContents [:Integer "1"]]
                       :scalar 1
                       :representation "1"}]
                     [{:content "2"
                       :ast [:CellContents [:Integer "2"]]
                       :scalar 2
                       :representation "2"}]]))
      (is (= (get-in evaluated-sheet [:grid 1 1 :scalar]) 1))
      (is (= (get-in evaluated-sheet [:grid 1 1 :spilled-into]) #{[1 1] [2 1]}))
      (is (= (get-in evaluated-sheet [:grid 2 1 :scalar]) 2))))

  (testing "Matrix spill errors if a cell has some content"
    (let [grid [["1" ""]
                ["2" "=A1:A2"]
                ["" "A string"]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))]
      (is (get-in evaluated-sheet [:grid 1 1 :matrix]))
      (is (= (get-in evaluated-sheet [:grid 1 1 :error]) "Spill error"))
      (is (= (get-in evaluated-sheet [:grid 2 1 :scalar]) "A string"))))

  (testing "Matrix spill errors if there's a conflict"
    (let [grid [["1" "3"]
                ["2" "=A1:A2"]
                ["=A1:B1" ""]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))]
      (is (= (get-in evaluated-sheet [:grid 1 1 :scalar]) 1))
      (is (= (get-in evaluated-sheet [:grid 2 1 :scalar]) 2))
      (is (= (get-in evaluated-sheet [:grid 2 0 :error]) "Spill error"))
      (is (= (get-in evaluated-sheet [:grid 2 0 :representation]) "Spill error"))
      (is (= (get-in evaluated-sheet [:grid 2 0 :scalar]) nil))
      (is (= (util/map-on-matrix :representation (:grid evaluated-sheet))
             [["1" "3"]
              ["2" "1"]
              ["Spill error" "2"]]))))

  (testing "Cells depending on spillages are evaluated"
    (let [grid (:grid (eval-sheet (new-sheet [["1" "" "=A1:A3"]
                                              ["2" "" ""]
                                              ["3" "=C2" ""]
                                              ["=B3" "=A3:A5" ""]
                                              ["" "" ""]
                                              ["" "" ""]]
                                             "")))]
      (is (= (get-in grid [2 1 :scalar]) 2))
      (is (= (get-in grid [3 0 :scalar]) 2))
      (is (= (get-in grid [3 1 :scalar]) 3))
      (is (= (get-in grid [4 1 :scalar]) 2))))

  (testing "Spill errors are re-evaluated when conflicting value is cleared"
    (let [sheet (eval-sheet (new-sheet [["1" "10" "20"   ""]
                                        ["2" ""   ""       "=A1:A3"]
                                        ["3" ""   "=A1:C1" "Cross"]]
                                       ""))
          evaluated-grid (:grid (eval-cell [2 3] sheet ""))]
      (is (= (get-in evaluated-grid [1 3 :scalar]) 1))
      (is (= (get-in evaluated-grid [2 3 :scalar]) 2))
      (is (= (get-in evaluated-grid [2 2 :error]) "Spill error"))))

  (testing "Function invocation"
    (is (= (util/map-on-matrix
            #(select-keys % [:scalar :content :error :representation])
            (:grid (eval-sheet (new-sheet [["1" "=concat(\"hello \" A1 A2)" ""]
                                           ["2" "" ""]
                                           ["=A1+A2" "" ""]
                                           ["=A3+1" "" ""]
                                           ["=A1+A2+A3+A4+10" "" ""]]
                                          ""))))
           [[{:content "1" :scalar 1 :representation "1"}
             {:content "=concat(\"hello \" A1 A2)" :scalar "hello 12" :representation "hello 12"}
             {:content "" :scalar "" :representation ""}]
            [{:content "2" :scalar 2 :representation "2"}
             {:content "" :scalar "" :representation ""}
             {:content "" :scalar "" :representation ""}]
            [{:content "=A1+A2" :scalar 3 :representation "3"}
             {:content "" :scalar "" :representation ""}
             {:content "" :scalar "" :representation ""}]
            [{:content "=A3+1" :scalar 4 :representation "4"}
             {:content "" :scalar "" :representation ""}
             {:content "" :scalar "" :representation ""}]
            [{:content "=A1+A2+A3+A4+10" :scalar 20 :representation "20"}
             {:content "" :scalar "" :representation ""}
             {:content "" :scalar "" :representation ""}]])))

  (testing "Inlined function invocation"
    (is (= (util/map-on-matrix
            #(select-keys % [:scalar :content :error :representation])
            (:grid (eval-sheet (new-sheet [["1" "={x+y+z}(9 A1 A2)"]
                                           ["2" ""]]
                                          ""))))
           [[{:content "1" :scalar 1 :representation "1"}
             {:content "={x+y+z}(9 A1 A2)" :scalar 12 :representation "12"}]
            [{:content "2" :scalar 2 :representation "2"}
             {:content "" :scalar "" :representation ""}]]))))

(deftest incremental-evaluate-grid
  (testing "Basic incremental evaluation given a pre-evaluated grid and a depgraph"
    (let [sheet (eval-sheet (new-sheet [["10" "=A1" "=A1+B1" "100" "=C1" "=A1"]]
                                       ""))
          {evaluated-grid :grid depgraph :depgraph} (eval-cell [0 1] sheet "=A1+D1")]
      (is (= 10 (:scalar (util/get-cell evaluated-grid [0 0]))))
      (is (= 110 (:scalar (util/get-cell evaluated-grid [0 1]))))
      (is (= 120 (:scalar (util/get-cell evaluated-grid [0 2]))))
      (is (= 100 (:scalar (util/get-cell evaluated-grid [0 3]))))
      (is (= 120 (:scalar (util/get-cell evaluated-grid [0 4]))))
      (is (= depgraph
             {[:cell [0 0]] #{[:cell [0 1]] [:cell [0 2]] [:cell [0 5]]}
              [:cell [0 1]] #{[:cell [0 2]]}
              [:cell [0 2]] #{[:cell [0 4]]}
              [:cell [0 3]] #{[:cell [0 1]]}}))))

  (testing "Older dependencies are removed in an incremental evaluation"
    (let [sheet (eval-sheet (new-sheet [["10" "=A1" "=A1+B1" "100"]]
                                       ""))
          {depgraph :depgraph} (eval-cell [0 1] sheet "=D1")]
      (is (= depgraph
             {[:cell [0 0]] #{[:cell [0 2]]}
              [:cell [0 1]] #{[:cell [0 2]]}
              [:cell [0 3]] #{[:cell [0 1]]}}))))

  (testing "Cells containing matrix references are spilled"
    (let [sheet (eval-sheet (new-sheet [["10" ""]
                                        ["20" ""]
                                        ["30" ""]]
                                       ""))
          {evaluated-grid :grid depgraph :depgraph} (eval-cell [0 1] sheet "=A1:A3")]
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["10" "10"]
              ["20" "20"]
              ["30" "30"]]))
      (is (get-in evaluated-grid [0 1 :matrix]))
      (is (= depgraph {[:cell [0 0]] #{[:cell [0 1]]}
                       [:cell [1 0]] #{[:cell [0 1]]}
                       [:cell [2 0]] #{[:cell [0 1]]}}))))

  (testing "If the address is referred somewhere in a matrix reference, the matrix reference is re-evaluated and spilled"
    (let [sheet (eval-sheet (new-sheet [["10" "=A1:A3"]
                                        ["" ""]
                                        ["" ""]
                                        ["=B2" ""]]
                                       ""))
          {evaluated-grid :grid depgraph :depgraph} (eval-cell [1 0] sheet "20")]
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["10" "10"]
              ["20" "20"]
              ["" ""]
              ["20" ""]]))))

  (testing "If content is added to a spilled cell, the origin results in a spill error"
    (let [sheet (eval-sheet (new-sheet [["10" "=A1:A3"]
                                        ["20" ""]
                                        ["" ""]]
                                       ""))
          {evaluated-grid :grid} (eval-cell [1 1] sheet "A string")]
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["10" "Spill error"]
              ["20" "A string"]
              ["" ""]]))))

  (testing "Unorderly references"
    (let [sheet (eval-sheet (new-sheet [["=B1:B4" "8"      "=D3" "19"]
                                        [""       "1"      "2"  "4"]
                                        [""       "=C1:D2" ""   "=C2+D2"]
                                        [""       "" ""   ""]]
                                       ""))
          {evaluated-grid :grid} (eval-cell [1 2] sheet "202")]
      (is (= (util/map-on-matrix :representation (:grid sheet))
             [["8" "8" "6" "19"]
              ["1" "1" "2" "4"]
              ["6" "6" "19" "6"]
              ["2" "2" "4" ""]]))
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["8" "8" "206" "19"]
              ["1" "1" "202" "4"]
              ["206" "206" "19" "206"]
              ["202" "202" "4" ""]]))))

  (testing "Styles are preserved after evaluation"
    (let [sheet (eval-sheet (new-sheet [["1" "=A1"]] ""))
          style {:background 0x000000}
          styled-sheet (assoc-in sheet [:grid 0 0 :style] style)
          {evaluated-grid :grid} (eval-cell [0 1] styled-sheet "=A1+1")]
      (is (= (get-in evaluated-grid [0 0 :style]) style)))))

(deftest depgraph-test
  (testing "Returns a reverse dependency graph for an evaluated grid"
    (is (= (make-depgraph (parse-grid [["10" "=A1" "=A1+B1" "=C1"]]))
           {[:cell [0 0]] #{[:cell [0 2]] [:cell [0 1]]}
            [:cell [0 1]] #{[:cell [0 2]]}
            [:cell [0 2]] #{[:cell [0 3]]}}))))

(deftest map-on-matrix-test
  (testing "Row order map of f over a 2D matrix"
    (let [matrix [[10 20 30]
                  [40 50 60]]]
      (is (= (util/map-on-matrix identity matrix) matrix)))))

(deftest map-on-matrix-addressed-test
  (testing "Row order map of f over a 2D matrix with address also supplied to f"
    (let [matrix [[10 20 30]
                  [40 50 60]]]
      (is (= (util/map-on-matrix-addressed
              (fn [address item] [address item]) matrix)
             [[[[0 0] 10] [[0 1] 20] [[0 2] 30]]
              [[[1 0] 40] [[1 1] 50] [[1 2] 60]]])))))

(deftest named-reference-evaluation-test
  (testing "A named reference is evaluated"
    (is (= [["1" "2" "3"]]
           (->> (new-sheet [["1" "2" "=addthree(1 1 1)"]] "addthree:{x+y+z}")
                eval-sheet
                :grid
                (util/map-on-matrix :representation)))))

  (testing "A named reference's dependents are re-evaluated"
    (is (= [["11" "2" "20"]]
           (as-> (new-sheet [["1" "2" "=addaone(9)"]] "addaone:{x+A1}") sheet
             (eval-sheet sheet)
             (eval-cell [0 0] sheet "=11")
             (util/map-on-matrix :representation (:grid sheet))))))

  (testing "A named reference is re-evaluated when its dependency changes"
    (is (= 10
           (as-> (new-sheet [["1" "2"]] "addaone:4+A1") sheet
             (eval-sheet sheet)
             (eval-cell [0 0] sheet "6")
             (get-in sheet [:bindings "addaone" :scalar])))))

  (testing "Depgraph is updated when a named reference's dependencies change"
    (is (= {[:cell [0 1]] #{[:named "addaone"]}}
           (as-> (new-sheet [["1" "2"]] "addaone:4+A1") sheet
             (eval-sheet sheet)
             (eval-code sheet "addaone:4+B1")
             (get-in sheet [:depgraph])))))

  (testing "Depgraph is updated when a named reference's dependents change"
    (is (= {[:named "addaone"] #{[:cell [0 1]]}}
           (as-> (new-sheet [["1" "2"]] "addaone:4") sheet
             (eval-sheet sheet)
             (eval-cell [0 1] sheet "=addaone+20")
             (get-in sheet [:depgraph]))))))

(deftest merge-cells-test
  (testing "Merges cells given an area"
    (let [sheet (-> (new-sheet (repeat 5 (repeat 5 "")) "")
                    eval-sheet
                    (grid/merge-cells {:start [0 0] :end [1 2]}))]
      (is (= (get-in sheet [:grid 0 0 :style]) {:merged-with [0 0]
                                                :merged-until [1 2]
                                                :merged-addresses #{[0 0] [0 1] [0 2] [1 0] [1 1] [1 2]}}))
      (is (= (area/cell-h sheet [0 0]) 2))
      (is (= (area/cell-w sheet [0 0]) 3))))

  (testing "Merges existing merged cells only if the full merged cell is in the area"
    (let [sheet (-> (new-sheet (repeat 5 (repeat 5 "")) "")
                    eval-sheet
                    (grid/merge-cells {:start [0 0] :end [1 1]})
                    (grid/merge-cells {:start [1 1] :end [1 2]}))]
      (is (= (select-keys (get-in sheet [:grid 0 0 :style])
                          [:merged-with :merged-until])
             {:merged-with [0 0]
              :merged-until [1 1]}))
      (is (= (select-keys (get-in (grid/merge-cells sheet {:start [0 0] :end [1 2]})
                                  [:grid 0 0 :style])
                          [:merged-with :merged-until])
             {:merged-with [0 0]
              :merged-until [1 2]}))))

  (testing "Merges labels into a single label"
    (let [table-name "A table"
          sheet (-> (new-sheet (repeat 5 (repeat 5 "")) "")
                    eval-sheet
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :top)
                    (grid/merge-cells {:start [0 1] :end [1 1]}))]
      (is (= (get-in sheet [:tables table-name :labels]) {[0 1] {:dirn :top :color nil}})))))
