(ns bean-tests.parser
  (:require [bean.parser :refer [parse-cell]]
            [clojure.test :refer [deftest testing is]]))

(deftest parser-test
  (testing "Basic Parsing"
    (is (= [:CellContents [:Integer "4"]]
           (parse-cell "4")))
    (is (= [:CellContents [:String "foo"]]
           (parse-cell "foo")))
    (is (= [:CellContents [:Expression [:Value [:Integer "89"]]]]
           (parse-cell "=89")))
    (is (= [:CellContents [:Expression [:Value [:QuotedString "foo"]]]]
           (parse-cell "=\"foo\"")))
    (is (= [:CellContents [:Expression [:CellRef "A" "8"]]]
           (parse-cell "=A8")))
    (is (= [:CellContents [:Expression
                           [:Expression [:CellRef "A" "8"]]
                           [:Operation "+"]
                           [:Expression [:CellRef "B" "9"]]]]
           (parse-cell "=A8+B9")))
    (is (= [:CellContents [:Expression
                           [:FunctionInvocation
                            [:Name "concat"]
                            [:Expression [:Value [:QuotedString "hello"]]]
                            [:Expression [:CellRef "A" "3"]]
                            [:Expression [:CellRef "A" "4"]]]]]
           (parse-cell "=concat(\"hello\" A3 A4)")))))