(ns bean.grid-test
  (:require [bean.grid :refer [parse-grid
                               eval-sheet
                               eval-code
                               eval-address
                               new-sheet]]
            [bean.deps :refer [make-depgraph]]
            [bean.util :as util]
            [clojure.test :refer [deftest testing is]]))

(deftest evaluator-test
  (testing "Basic evaluation"
    (let [grid [["1" "" ""]
                ["2" "" ""]
                ["=A1+A2" "" ""]
                ["=A3+1" "" ""]
                ["=A1+A2+A3+A4+10" "" ""]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))]
      (is (= (util/map-on-matrix
              #(select-keys % [:value :content :error :representation])
              (:grid evaluated-sheet))
             [[{:content "1" :value 1 :representation "1"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]
              [{:content "2" :value 2 :representation "2"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]
              [{:content "=A1+A2" :value 3 :representation "3"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]
              [{:content "=A3+1" :value 4 :representation "4"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]
              [{:content "=A1+A2+A3+A4+10" :value 20 :representation "20"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]]))
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
              #(select-keys % [:value :content :error :representation])
              (:grid (eval-sheet (new-sheet grid ""))))
             [[{:content "=1" :value 1 :representation "1"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]
              [{:content "ABC" :value "ABC" :representation "ABC"}
               {:content "=A1000" :value nil :error "Invalid address [999 0]" :representation "Invalid address [999 0]"}
               {:content "" :value nil :representation ""}]
              [{:content "=A1+A2" :value nil :error "+ only works for Integers" :representation "+ only works for Integers"}
               {:content "=B2" :value nil :error "Invalid address [999 0]" :representation "Invalid address [999 0]"}
               {:content "" :value nil :representation ""}]
              [{:content "=A3+1" :value nil :error "+ only works for Integers" :representation "+ only works for Integers"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]]))))

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
                       :value 1
                       :representation "1"}]
                     [{:content "2"
                       :ast [:CellContents [:Integer "2"]]
                       :value 2
                       :representation "2"}]]))
      (is (= (get-in evaluated-sheet [:grid 1 1 :value]) 1))
      (is (= (get-in evaluated-sheet [:grid 1 1 :spilled-into]) #{[1 1] [2 1]}))
      (is (= (get-in evaluated-sheet [:grid 2 1 :value]) 2))))

  (testing "Matrix spill errors if a cell has some content"
    (let [grid [["1" ""]
                ["2" "=A1:A2"]
                ["" "A string"]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))]
      (is (get-in evaluated-sheet [:grid 1 1 :matrix]))
      (is (= (get-in evaluated-sheet [:grid 1 1 :error]) "Spill error"))
      (is (= (get-in evaluated-sheet [:grid 2 1 :value]) "A string"))))

  (testing "Matrix spill errors if there's a conflict"
    (let [grid [["1" "3"]
                ["2" "=A1:A2"]
                ["=A1:B1" ""]]
          evaluated-sheet (eval-sheet (new-sheet grid ""))]
      (is (= (get-in evaluated-sheet [:grid 1 1 :value]) 1))
      (is (= (get-in evaluated-sheet [:grid 2 1 :value]) 2))
      (is (= (get-in evaluated-sheet [:grid 2 0 :error]) "Spill error"))
      (is (= (get-in evaluated-sheet [:grid 2 0 :representation]) "Spill error"))
      (is (= (get-in evaluated-sheet [:grid 2 0 :value]) nil))
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
      (is (= (get-in grid [2 1 :value]) 2))
      (is (= (get-in grid [3 0 :value]) 2))
      (is (= (get-in grid [3 1 :value]) 3))
      (is (= (get-in grid [4 1 :value]) 2))))

  (testing "Function invocation"
    (is (= (util/map-on-matrix
            #(select-keys % [:value :content :error :representation])
            (:grid (eval-sheet (new-sheet [["1" "=concat(\"hello \" A1 A2)" ""]
                                           ["2" "" ""]
                                           ["=A1+A2" "" ""]
                                           ["=A3+1" "" ""]
                                           ["=A1+A2+A3+A4+10" "" ""]]
                                          ""))))
           [[{:content "1" :value 1 :representation "1"}
             {:content "=concat(\"hello \" A1 A2)" :value "hello 12" :representation "hello 12"}
             {:content "" :value nil :representation ""}]
            [{:content "2" :value 2 :representation "2"}
             {:content "" :value nil :representation ""}
             {:content "" :value nil :representation ""}]
            [{:content "=A1+A2" :value 3 :representation "3"}
             {:content "" :value nil :representation ""}
             {:content "" :value nil :representation ""}]
            [{:content "=A3+1" :value 4 :representation "4"}
             {:content "" :value nil :representation ""}
             {:content "" :value nil :representation ""}]
            [{:content "=A1+A2+A3+A4+10" :value 20 :representation "20"}
             {:content "" :value nil :representation ""}
             {:content "" :value nil :representation ""}]])))

  (testing "Inlined function invocation"
    (is (= (util/map-on-matrix
            #(select-keys % [:value :content :error :representation])
            (:grid (eval-sheet (new-sheet [["1" "={x+y+z}(9 A1 A2)"]
                                           ["2" ""]]
                                          ""))))
           [[{:content "1" :value 1 :representation "1"}
             {:content "={x+y+z}(9 A1 A2)" :value 12 :representation "12"}]
            [{:content "2" :value 2 :representation "2"}
             {:content "" :value nil :representation ""}]]))))

(deftest incremental-evaluate-grid
  (testing "Basic incremental evaluation given a pre-evaluated grid and a depgraph"
    (let [sheet (eval-sheet (new-sheet [["10" "=A1" "=A1+B1" "100" "=C1" "=A1"]]
                                       ""))
          {evaluated-grid :grid depgraph :depgraph} (eval-address [:cell [0 1]] sheet "=A1+D1")]
      (is (= 10 (:value (util/get-cell evaluated-grid [0 0]))))
      (is (= 110 (:value (util/get-cell evaluated-grid [0 1]))))
      (is (= 120 (:value (util/get-cell evaluated-grid [0 2]))))
      (is (= 100 (:value (util/get-cell evaluated-grid [0 3]))))
      (is (= 120 (:value (util/get-cell evaluated-grid [0 4]))))
      (is (= depgraph
             {[:cell [0 0]] #{[:cell [0 1]] [:cell [0 2]] [:cell [0 5]]}
              [:cell [0 1]] #{[:cell [0 2]]}
              [:cell [0 2]] #{[:cell [0 4]]}
              [:cell [0 3]] #{[:cell [0 1]]}}))))

  (testing "Older dependencies are removed in an incremental evaluation"
    (let [sheet (eval-sheet (new-sheet [["10" "=A1" "=A1+B1" "100"]]
                                       ""))
          {depgraph :depgraph} (eval-address [:cell [0 1]] sheet "=D1")]
      (is (= depgraph
             {[:cell [0 0]] #{[:cell [0 2]]}
              [:cell [0 1]] #{[:cell [0 2]]}
              [:cell [0 3]] #{[:cell [0 1]]}}))))

  (testing "Cells containing matrix references are spilled"
    (let [sheet (eval-sheet (new-sheet [["10" ""]
                                        ["20" ""]
                                        ["30" ""]]
                                       ""))
          {evaluated-grid :grid depgraph :depgraph} (eval-address [:cell [0 1]] sheet "=A1:A3")]
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
          {evaluated-grid :grid depgraph :depgraph} (eval-address [:cell [1 0]] sheet "20")]
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
          {evaluated-grid :grid} (eval-address [:cell [1 1]] sheet "A string")]
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
          {evaluated-grid :grid} (eval-address [:cell [1 2]] sheet "202")]
      (is (= (util/map-on-matrix :representation (:grid sheet))
             [["8" "8" "6" "19"]
              ["1" "1" "2" "4"]
              ["6" "6" "19" "6"]
              ["2" "2" "4" ""]]))
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["8" "8" "206" "19"]
              ["1" "1" "202" "4"]
              ["206" "206" "19" "206"]
              ["202" "202" "4" ""]])))))

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
             (eval-address [:cell [0 0]] sheet "=11")
             (util/map-on-matrix :representation (:grid sheet))))))

  (testing "A named reference is re-evaluated when its dependency changes"
    (is (= 10
           (as-> (new-sheet [["1" "2"]] "addaone:4+A1") sheet
             (eval-sheet sheet)
             (eval-address [:cell [0 0]] sheet "6")
             (get-in sheet [:bindings "addaone" :value])))))

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
             (eval-address [:cell [0 1]] sheet "=addaone+20")
             (get-in sheet [:depgraph]))))))
