(ns bean-tests.core
  (:require [bean.core :refer [parse evaluate-grid]]
            [clojure.test :refer [deftest testing is]]))

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
    (is (= {[0 0] {:content "1", :value 1, :affected-cells #{[0 0]}},
            [1 0] {:content "2", :value 2, :affected-cells #{[1 0]}},
            [2 0] {:content "3", :value 3, :affected-cells #{[2 0]}},
            [3 0] {:content "4", :value 4, :affected-cells #{[3 0]}},
            [4 0] {:content "20", :value 20, :affected-cells #{[4 0]}}}
           (let [grid {[0 0] "1"
                       [1 0] "2"
                       [2 0] "=A1+A2"
                       [3 0] "=A3+1"
                       [4 0] "=A1+A2+A3+A4+10"}]
             (evaluate-grid grid))))))

