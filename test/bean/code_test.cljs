(ns bean.code-test
  (:require [clojure.test :refer [deftest testing is]]
            [bean.code :as code]
            [bean.grid :as grid]))

(deftest code-errors-test
  (testing "When the code has a parse error, it is escalated on reevaluation"
    (is (= (-> (grid/new-sheet [[""]] "")
               (code/set-code "foo: 99fail")
               code/reevaluate
               :code-error)
           "Parse Error: idx 7. [{:tag :string, :expecting \"*\"} {:tag :regexp, :expecting #\"^\\s+\"} {:tag :string, :expecting \"+\"} {:tag :string, :expecting \".\"} {:tag :string, :expecting \"\\n\"}]")))

  (testing "When the code has an error in a statement, it is escalated on reevaluation"
    (is (= (-> (grid/new-sheet [[""]] "")
               (code/set-code "foo: B3")
               code/reevaluate
               :code-error)
           "name: foo. Invalid address [2 1]")))

  (testing "When the code has an error in a statement, it is escalated on initial
            evaluation after the grid is evaluated"
    (is (= (-> (grid/new-sheet [["1"]] "foo: A1+\"bar\"")
               grid/eval-sheet
               :code-error)
           "name: foo. + only works for Integers")))

  (testing "When the code has an error in a statement, it is escalated on initial
            evaluation after the grid is evaluated but is cleared in a subsequent
            evaluation where the error is resolved"
    (is (nil? (-> (grid/new-sheet [["1"]] "foo: A1+\"bar\"")
                  grid/eval-sheet
                  (code/set-code "foo: A1+20")
                  code/reevaluate
                  :code-error)))))
