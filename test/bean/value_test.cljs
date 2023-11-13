(ns bean.value-test
  (:require [clojure.test :refer [deftest is testing]]
            [bean.value :as value]))

(deftest new-test
  (testing "Returns a new value for a statement with given content & ast"
    (is (= {:ast [:Expression
                  [:Expression [:CellRef "A" "8"]]
                  [:Operation "+"]
                  [:Expression [:CellRef "B" "9"]]]
            :content "A8+B9"}
           (value/from-statement "A8+B9" [:Expression
                                          [:Expression [:CellRef "A" "8"]]
                                          [:Operation "+"]
                                          [:Expression [:CellRef "B" "9"]]]))))
  (testing "Returns a value that parses given cell contents & uses that ast"
    (is (= {:ast [:CellContents [:Expression
                                 [:Expression [:CellRef "A" "8"]]
                                 [:Operation "+"]
                                 [:Expression [:CellRef "B" "9"]]]]
            :content "=A8+B9"}
           (value/from-cell "=A8+B9")))))