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
    (is (= [:CellContents [:UserExpression [:Expression [:Ref "A8"]]]]
           (parse "=A8")))
    (is (= [:CellContents [:UserExpression
                           [:Expression
                            [:Expression [:Ref "A8"]]
                            [:Operation "+"]
                            [:Expression [:Ref "B9"]]]]]
           (parse "=A8+B9")))
    (is (= [:CellContents [:UserExpression
                           [:Expression
                            [:FunctionInvocation
                             [:FunctionName "concat"]
                             [:FunctionArguments
                              [:Expression [:Value [:QuotedString [:QuotedRawString "hello"]]]]
                              [:FunctionArguments
                               [:Expression [:Ref "A3"]]
                               [:FunctionArguments [:Expression [:Ref "A4"]]]]]]]]]
           (parse "=concat(\"hello\",A3,A4)")))))

(deftest evaluator-test
  (testing "Basic evaluation"
    (is (= {"A1" {:content "1", :value 1, :affected-cells #{"A1"}},
            "A2" {:content "2", :value 2, :affected-cells #{"A2"}},
            "A3" {:content "3", :value 3, :affected-cells #{"A3"}},
            "A4" {:content "4", :value 4, :affected-cells #{"A4"}},
            "A5" {:content "20", :value 20, :affected-cells #{"A5"}}}
           (let [grid {"A1" "1"
                       "A2" "2"
                       "A3" "=A1+A2"
                       "A4" "=A3+1"
                       "A5" "=A1+A2+A3+A4+10"}]
             (evaluate-grid grid))))))

