(ns bean-tests.core
  (:require [bean.core :refer [parse evaluate-grid
                               bean-op-+
                               map-on-matrix
                               map-on-matrix-addressed]]
            [clojure.test :refer [deftest testing is run-all-tests]]))

(deftest parser-test
  (testing "Basic Parsing"
    (is (= [:CellContents [:Constant [:Integer "4"]]]
           (parse "4")))
    (is (= [:CellContents [:Constant [:String "foo"]]]
           (parse "foo")))
    (is (= [:CellContents [:UserExpression [:Expression [:Value [:Integer "89"]]]]]
           (parse "=89")))
    (is (= [:CellContents [:UserExpression [:Expression [:Value [:QuotedString [:QuotedRawString "foo"]]]]]]
           (parse "=\"foo\"")))
    (is (= [:CellContents [:UserExpression [:Expression [:CellRef "A" "8"]]]]
           (parse "=A8")))
    (is (= [:CellContents [:UserExpression
                           [:Expression
                            [:Expression [:CellRef "A" "8"]]
                            [:Operation "+"]
                            [:Expression [:CellRef "B" "9"]]]]]
           (parse "=A8+B9")))
    (is (= [:CellContents [:UserExpression
                           [:Expression
                            [:FunctionInvocation
                             [:FunctionName "concat"]
                             [:FunctionArguments
                              [:Expression [:Value [:QuotedString [:QuotedRawString "hello"]]]]
                              [:FunctionArguments
                               [:Expression [:CellRef "A" "3"]]
                               [:FunctionArguments [:Expression [:CellRef "A" "4"]]]]]]]]]
           (parse "=concat(\"hello\",A3,A4)")))))

(deftest evaluator-test
  (testing "Basic evaluation"
    (let [grid [["1" "" ""]
                ["2" "" ""]
                ["=A1+A2" "" ""]
                ["=A3+1" "" ""]
                ["=A1+A2+A3+A4+10" "" ""]]]
      (is (= (evaluate-grid grid)
             [[{:content "1" :value 1, :affected-cells #{[0 0]}}
               {:content "" :value nil, :affected-cells #{[0 1]}}
               {:content "" :value nil, :affected-cells #{[0 2]}}]
              [{:content "2" :value 2, :affected-cells #{[1 0]}}
               {:content "" :value nil, :affected-cells #{[1 1]}}
               {:content "" :value nil, :affected-cells #{[1 2]}}]
              [{:content "3" :value 3, :affected-cells #{[2 0]}}
               {:content "" :value nil, :affected-cells #{[2 1]}}
               {:content "" :value nil, :affected-cells #{[2 2]}}]
              [{:content "4" :value 4, :affected-cells #{[3 0]}}
               {:content "" :value nil, :affected-cells #{[3 1]}}
               {:content "" :value nil, :affected-cells #{[3 2]}}]
              [{:content "20" :value 20, :affected-cells #{[4 0]}}
               {:content "" :value nil, :affected-cells #{[4 1]}}
               {:content "" :value nil, :affected-cells #{[4 2]}}]]))))

  (testing "Returns errors"
    (let [grid [["=1" "" ""]
                ["ABC" "" ""]
                ["=A1+A2" "" ""]
                ["=A3+1" "" ""]]]
      (is (= (evaluate-grid grid)
             [[{:content "1" :value 1 :affected-cells #{[0 0]}}
               {:content "" :value nil :affected-cells #{[0 1]}}
               {:content "" :value nil :affected-cells #{[0 2]}}]
              [{:content "ABC" :value "ABC" :affected-cells #{[1 0]}}
               {:content "" :value nil :affected-cells #{[1 1]}}
               {:content "" :value nil :affected-cells #{[1 2]}}]
              [{:content "" :value nil :error "Addition only works for Integers" :affected-cells #{[2 0]}}
               {:content "" :value nil :affected-cells #{[2 1]}}
               {:content "" :value nil :affected-cells #{[2 2]}}]
              [{:content "" :value nil :error "Addition only works for Integers" :affected-cells #{[3 0]}}
               {:content "" :value nil :affected-cells #{[3 1]}}
               {:content "" :value nil :affected-cells #{[3 2]}}]])))))

(deftest bean-op-+-test
  (testing "Adds two numbers"
    (is (= (bean-op-+ 2 3) 5)))
  (testing "Returns an error if an operand is an invalid data type"
    (is (= (bean-op-+ "1" 2) {:error "Addition only works for Integers"}))))

(deftest map-on-matrix-test
  (testing "Row order map of f over the grid"
    (let [grid [[10 20 30]
                [40 50 60]]]
      (is (= (map-on-matrix identity grid) grid)))))

(deftest map-on-matrix-addressed-test
  (testing "Row order map over the grid with the address of each cell passed to f"
    (let [grid [[10 20 30]
                [40 50 60]]]
      (is (= (map-on-matrix-addressed
              (fn [address contents] [address contents]) grid)
             [[[[0 0] 10] [[0 1] 20] [[0 2] 30]]
              [[[1 0] 40] [[1 1] 50] [[1 2] 60]]])))))