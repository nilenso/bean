(ns bean-tests.parser
  (:require [bean.parser :refer [parse]]
            [clojure.test :refer [deftest testing is]]))

(deftest parser-test
  (testing "Basic Parsing"
    (is (= [:CellContents [:Integer "4"]]
           (parse "4")))
    (is (= [:CellContents [:String "foo"]]
           (parse "foo")))
    (is (= [:CellContents [:Expression [:Value [:Integer "89"]]]]
           (parse "=89")))
    (is (= [:CellContents [:Expression [:Value [:QuotedString "foo"]]]]
           (parse "=\"foo\"")))
    (is (= [:CellContents [:Expression [:CellRef "A" "8"]]]
           (parse "=A8")))
    (is (= [:CellContents [:Expression
                           [:Expression [:CellRef "A" "8"]]
                           [:Operation "+"]
                           [:Expression [:CellRef "B" "9"]]]]
           (parse "=A8+B9")))
    (is (= [:CellContents [:Expression
                           [:FunctionInvocation
                            [:FunctionName "concat"]
                            [:FunctionArguments
                             [:Expression [:Value [:QuotedString "hello"]]]
                             [:FunctionArguments
                              [:Expression [:CellRef "A" "3"]]
                              [:FunctionArguments [:Expression [:CellRef "A" "4"]]]]]]]]
           (parse "=concat(\"hello\",A3,A4)")))))