(ns bean-tests.core
  (:require [bean.core :refer [parse evaluate-grid
                               bean-op-+
                               map-on-matrix
                               map-on-matrix-addressed]]
            [clojure.test :refer [deftest testing is run-all-tests]]))

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

(deftest evaluator-test
  (testing "Basic evaluation"
    (let [grid [["1" "" ""]
                ["2" "" ""]
                ["=A1+A2" "" ""]
                ["=A3+1" "" ""]
                ["=A1+A2+A3+A4+10" "" ""]]]
      (is (= (map-on-matrix
              #(select-keys % [:value :content :error :representation])
              (evaluate-grid grid))
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
               {:content "" :value nil :representation ""}]]))))

  (testing "Returns errors"
    (let [grid [["=1" "" ""]
                ["ABC" "=A1000" ""]
                ["=A1+A2" "=B2" ""]
                ["=A3+1" "" ""]]]
      (is (= (map-on-matrix
              #(select-keys % [:value :content :error :representation])
              (evaluate-grid grid))
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
              (evaluate-grid grid))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {}]]))))

  (testing "If a cell has an error, all dependent cells become errors"
    (let [grid [["=A1000" "=A1" "=B1"]]]
      (is (= (map-on-matrix
              #(select-keys % [:error])
              (evaluate-grid grid))
             [[{:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}
               {:error "Invalid address [999 0]"}]])))))

(deftest bean-op-+-test
  (testing "Adds two numbers"
    (is (= (bean-op-+ 2 3) 5)))
  (testing "Returns an error if an operand is an invalid data type"
    (is (= (bean-op-+ "1" 2) {:error "Addition only works for Integers"}))))

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
