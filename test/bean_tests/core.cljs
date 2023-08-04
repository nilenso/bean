(ns bean-tests.core
  (:require [bean.core :refer [parse]]
            [clojure.test :refer :all]
            [instaparse.core :as insta]))

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
           (parse "=A8")))))
