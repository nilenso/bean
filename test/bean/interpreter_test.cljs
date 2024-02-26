(ns bean.interpreter-test
  (:require [bean.grid :as grid]
            [bean.interpreter :as interpreter]
            [bean.operators :as operators]
            [bean.parser.parser :as parser]
            [clojure.test :refer [deftest is testing]]))

(defn- new-sheet []
  (grid/eval-sheet (grid/new-sheet (repeat 5 (repeat 5 "")) "")))

(deftest apply-op-test
  (testing "Applies an op to a matrix and a scalar"
    (is (= (interpreter/apply-op {:scalar operators/bean-op-+} {:scalar 1}
                                 {:matrix [[{:scalar 1}
                                            {:scalar 2}
                                            {:scalar 1}]]})
           {:matrix
            [[{:scalar 2 :representation "2"}
              {:scalar 3 :representation "3"}
              {:scalar 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (interpreter/apply-op {:scalar operators/bean-op-+}
                                 {:matrix [[{:scalar 1}
                                            {:scalar 2}
                                            {:scalar 1}]]}
                                 {:scalar 1})
           {:matrix
            [[{:scalar 2 :representation "2"}
              {:scalar 3 :representation "3"}
              {:scalar 2 :representation "2"}]]})))

  (testing "Applies an op to a scalar and a matrix"
    (is (= (interpreter/apply-op {:scalar operators/bean-op-+}
                                 {:matrix [[{:scalar 1}
                                            {:scalar 2}
                                            {:scalar 1}]]}
                                 {:matrix [[{:scalar 1}
                                            {:scalar 2}
                                            {:scalar 1}]]})
           {:matrix
            [[{:scalar 2 :representation "2"}
              {:scalar 4 :representation "4"}
              {:scalar 2 :representation "2"}]]}))))

(deftest function-chain-test
  (testing "Passes expression as the first argument to function at end of chain"
    (let [sheet (update-in
                 (new-sheet) [:bindings]
                 merge
                 {"one" {:content "{1}"
                         :ast [:Expression [:FunctionDefinition [:Expression [:Value [:Integer "1"]]]]]
                         :scalar [:Expression [:Value [:Integer "1"]]]
                         :representation ""}
                  "inc" {:content "{x+1}"
                         :ast [:Expression [:FunctionDefinition [:Expression [:Expression [:Name "x"]] [:Operation "+"] [:Expression [:Value [:Integer "1"]]]]]]
                         :scalar [:Expression [:Expression [:Name "x"]] [:Operation "+"] [:Expression [:Value [:Integer "1"]]]]
                         :representation ""}
                  "add" {:content "{x+y}"
                         :ast [:Expression [:FunctionDefinition [:Expression [:Expression [:Name "x"]] [:Operation "+"] [:Expression [:Name "y"]]]]]
                         :scalar [:Expression [:Expression [:Name "x"]] [:Operation "+"] [:Expression [:Name "y"]]]
                         :representation ""}})]
      (is (= (:scalar (interpreter/eval-ast (parser/parse "=one().inc()") sheet)) 2))
      (is (= (:scalar (interpreter/eval-ast (parser/parse "=one().inc().add(20)") sheet)) 22)))))
