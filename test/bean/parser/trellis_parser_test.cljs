(ns bean.parser.trellis-parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [bean.parser.trellis-parser :as trellis-parser]))

(deftest trellis-parser-test
  (testing "Leaf file format parsing"
    (is (= (trellis-parser/parse (str/join "\n\n%\n\n" ["foo:10"
                                                        "1,2\n3,4"
                                                        "A1= 1"]))
           [:TrellisFile
            [:Program
             [:LetStatement
              [:Name "foo"]
              [:Expression [:Value [:Number "10"]]]]]
            [["1" "2"] ["3" "4"]]
            [:TestProgram
             [:AssertionStatement
              [:Expression [:CellRef "A" "1"]]
              [:Expression [:Value [:Number "1"]]]]]])))
  (testing "Parsing tests code"
    (is (= (trellis-parser/parse-tests "A1=8\n1+1=10")
           [:TestProgram
            [:AssertionStatement
             [:Expression [:CellRef "A" "1"]]
             [:Expression [:Value [:Number "8"]]]]
            [:AssertionStatement
             [:Expression [:Expression [:Value [:Number "1"]]] [:Operation "+"] [:Expression [:Value [:Number "1"]]]]
             [:Expression [:Value [:Number "10"]]]]]))))
