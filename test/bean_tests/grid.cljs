(ns bean-tests.grid
  (:require [bean.grid :refer [evaluate-grid
                               map-on-matrix
                               map-on-matrix-addressed
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
      (is (= (map-on-matrix
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
      (is (= (map-on-matrix
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
      (is (= (map-on-matrix
              #(select-keys % [:error])
              (:grid (evaluate-grid grid)))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {}]]))))

  (testing "If a cell has an error, all dependent cells become errors"
    (let [grid [["=A1000" "=A1" "=B1"]]]
      (is (= (map-on-matrix
              #(select-keys % [:error])
              (:grid (evaluate-grid grid)))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}]])))))

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
              [0 3] #{[0 1]}})))))

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
      (is (= (map-on-matrix identity matrix) matrix)))))

(deftest map-on-matrix-addressed-test
  (testing "Row order map of f over a 2D matrix with address also supplied to f"
    (let [matrix [[10 20 30]
                  [40 50 60]]]
      (is (= (map-on-matrix-addressed
              (fn [address item] [address item]) matrix)
             [[[[0 0] 10] [[0 1] 20] [[0 2] 30]]
              [[[1 0] 40] [[1 1] 50] [[1 2] 60]]])))))
