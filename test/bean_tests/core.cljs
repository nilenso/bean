(ns bean-tests.core
  (:require [bean.core :refer [parse]]
            [clojure.test :refer [deftest testing is]]))

(deftest parser-test
  (testing "Basic Parsing"
    (is (= [:CellContents [:Constant [:Integer "4"]]]
           (parse "4")))
    (is (= [:CellContents [:Constant [:String "foo"]]]
           (parse "foo")))
    (is (= [:CellContents [:UserExpression "=" [:Expression [:Value [:Integer "89"]]]]]
           (parse "=89")))
    (is (= [:CellContents [:UserExpression "=" [:Expression [:Value [:QuotedString "\"" [:QuotedRawString "foo"] "\""]]]]]
           (parse "=\"foo\"")))
    (is (= [:CellContents [:UserExpression "=" [:Expression [:CellAddress "A8"]]]]
           (parse "=A8")))
    (is (= [:CellContents [:UserExpression "="
                           [:Expression
                            [:Expression [:CellAddress "A8"]]
                            [:Operation "+"]
                            [:Expression [:CellAddress "B9"]]]]]
           (parse "=A8+B9")))
    (is (= [:CellContents [:UserExpression "="
                           [:Expression
                            [:FunctionInvocation
                             [:FunctionName "concat"]
                             "("
                             [:FunctionArguments
                              [:Expression [:Value [:QuotedString "\"" [:QuotedRawString "hello"] "\""]]]
                              ","
                              [:FunctionArguments
                               [:Expression [:CellAddress "A3"]]
                               ","
                               [:FunctionArguments [:Expression [:CellAddress "A4"]]]]]
                             ")"]]]]
           (parse "=concat(\"hello\",A3,A4)")))))