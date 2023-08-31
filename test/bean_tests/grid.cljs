(ns bean-tests.grid
  (:require [bean.grid :refer [evaluate-grid
                               depgraph]]
            [bean.util :as util]
            [clojure.test :refer [deftest testing is]]))

(deftest evaluator-test
  (testing "Basic evaluation"
    (let [grid [["1" "" ""]
                ["2" "" ""]
                ["=A1+A2" "" ""]
                ["=A3+1" "" ""]
                ["=A1+A2+A3+A4+10" "" ""]]
          evaluated-grid (evaluate-grid grid)]
      (is (= (util/map-on-matrix
              #(select-keys % [:value :content :error :representation])
              (:grid evaluated-grid))
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
      (is (= (:depgraph (evaluate-grid grid))
             {[0 0] #{[2 0] [4 0]}
              [1 0] #{[2 0] [4 0]}
              [2 0] #{[3 0] [4 0]}
              [3 0] #{[4 0]}}))))

  (testing "Returns errors"
    (let [grid [["=1" "" ""]
                ["ABC" "=A1000" ""]
                ["=A1+A2" "=B2" ""]
                ["=A3+1" "" ""]]]
      (is (= (util/map-on-matrix
              #(select-keys % [:value :content :error :representation])
              (:grid (evaluate-grid grid)))
             [[{:content "=1" :value 1 :representation "1"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]
              [{:content "ABC" :value "ABC" :representation "ABC"}
               {:content "=A1000" :value nil :error "Invalid address [999 0]" :representation "Invalid address [999 0]"}
               {:content "" :value nil :representation ""}]
              [{:content "=A1+A2" :value nil :error "Addition only works for Integers" :representation "Addition only works for Integers"}
               {:content "=B2" :value nil :error "Invalid address [999 0]" :representation "Invalid address [999 0]"}
               {:content "" :value nil :representation ""}]
              [{:content "=A3+1" :value nil :error "Addition only works for Integers" :representation "Addition only works for Integers"}
               {:content "" :value nil :representation ""}
               {:content "" :value nil :representation ""}]]))))

  (testing "Errors are not operated upon further"
    (let [grid [["=A1000+1" "=A1+100" ""]]]
      (is (= (util/map-on-matrix
              #(select-keys % [:error])
              (:grid (evaluate-grid grid)))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {}]]))))

  (testing "If a cell has an error, all dependent cells become errors"
    (let [grid [["=A1000" "=A1" "=B1"]]]
      (is (= (util/map-on-matrix
              #(select-keys % [:error])
              (:grid (evaluate-grid grid)))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}]]))))

  (testing "Matrix evaluation"
    (let [grid [["1" ""]
                ["2" "=A1:A2"]
                ["" ""]]
          evaluated-grid (evaluate-grid grid)
          matrix (get-in evaluated-grid [:grid 1 1 :matrix])]
      (is (= matrix [[{:content "1"
                       :ast [:CellContents [:Integer "1"]]
                       :value 1
                       :representation "1"}]
                     [{:content "2"
                       :ast [:CellContents [:Integer "2"]]
                       :value 2
                       :representation "2"}]]))
      (is (= (get-in evaluated-grid [:grid 1 1 :value]) 1))
      (is (= (get-in evaluated-grid [:grid 2 1 :value]) 2))))

  (testing "Matrix spill errors if a cell has some content"
    (let [grid [["1" ""]
                ["2" "=A1:A2"]
                ["" "A string"]]
          evaluated-grid (evaluate-grid grid)]
      (is (get-in evaluated-grid [:grid 1 1 :matrix]))
      (is (= (get-in evaluated-grid [:grid 1 1 :error]) "Spill error"))
      (is (= (get-in evaluated-grid [:grid 2 1 :value]) "A string"))))

  (testing "Matrix spill errors if there's a conflict"
    (let [grid [["1" "3"]
                ["2" "=A1:A2"]
                ["=A1:B1" ""]]
          evaluated-grid (evaluate-grid grid)]
      (is (= (get-in evaluated-grid [:grid 1 1 :value]) 1))
      (is (= (get-in evaluated-grid [:grid 2 1 :value]) 2))
      (is (= (get-in evaluated-grid [:grid 2 0 :error]) "Spill error"))
      (is (= (get-in evaluated-grid [:grid 2 0 :representation]) "Spill error"))
      (is (= (get-in evaluated-grid [:grid 2 0 :value]) nil))
      (is (= (util/map-on-matrix :representation (:grid evaluated-grid))
             [["1" "3"]
              ["2" "1"]
              ["Spill error" "2"]])))))

(deftest incremental-evaluate-grid
  (testing "Basic incremental evaluation given a pre-evaluated grid and a depgraph"
    (let [grid (evaluate-grid [["10" "=A1" "=A1+B1" "100" "=C1" "=A1"]])
          {evaluated-grid :grid depgraph :depgraph} (evaluate-grid [0 1] "=A1+D1" (:grid grid) (:depgraph grid))]
      (is (= 10 (:value (util/get-cell evaluated-grid [0 0]))))
      (is (= 110 (:value (util/get-cell evaluated-grid [0 1]))))
      (is (= 120 (:value (util/get-cell evaluated-grid [0 2]))))
      (is (= 100 (:value (util/get-cell evaluated-grid [0 3]))))
      (is (= 120 (:value (util/get-cell evaluated-grid [0 4]))))
      (is (= depgraph
             {[0 0] #{[0 1] [0 2] [0 5]}
              [0 1] #{[0 2]}
              [0 2] #{[0 4]}
              [0 3] #{[0 1]}}))))

  (testing "Older dependencies are removed in an incremental evaluation"
    (let [grid (evaluate-grid [["10" "=A1" "=A1+B1" "100"]])
          {depgraph :depgraph} (evaluate-grid [0 1] "=D1" (:grid grid) (:depgraph grid))]
      (is (= depgraph
             {[0 0] #{[0 1] [0 2]}
              [0 1] #{[0 2]}
              [0 3] #{[0 1]}}))))

  (testing "Cells containing matrix references are spilled"
    (let [grid (evaluate-grid [["10" ""]
                               ["20" ""]
                               ["30" ""]])
          {evaluated-grid :grid depgraph :depgraph} (evaluate-grid [0 1] "=A1:A3" (:grid grid) (:depgraph grid))]
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["10" "10"]
              ["20" "20"]
              ["30" "30"]]))
      (is (get-in evaluated-grid [0 1 :matrix]))
      (is (= depgraph {[0 0] #{[0 1]}
                       [1 0] #{[0 1]}
                       [2 0] #{[0 1]}}))))

  (testing "If the address is referred somewhere in a matrix reference, the matrix reference is re-evaluated and spilled"
    (let [grid (evaluate-grid [["10" "=A1:A3"]
                               ["" ""]
                               ["" ""]])
          {evaluated-grid :grid depgraph :depgraph} (evaluate-grid [1 0] "20" (:grid grid) (:depgraph grid))]
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["10" "10"]
              ["20" "20"]
              ["" ""]]))))

  (testing "If content is added to a spilled cell, the origin results in a spill error"
    (let [grid (evaluate-grid [["10" "=A1:A3"]
                               ["20" ""]
                               ["" ""]])
          {evaluated-grid :grid} (evaluate-grid [1 1] "A string" (:grid grid) (:depgraph depgraph))]
      (is (= (util/map-on-matrix :representation evaluated-grid)
             [["10" "Spill error"]
              ["" "A string"]
              ["" ""]])))))

(deftest depgraph-test
  (testing "Returns a reverse dependency graph for an evaluated grid"
    (is (= (depgraph (:grid (evaluate-grid [["10" "=A1" "=A1+B1" "=C1"]])))
           {[0 0] #{[0 2] [0 1]}
            [0 1] #{[0 2]}
            [0 2] #{[0 3]}}))))

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
