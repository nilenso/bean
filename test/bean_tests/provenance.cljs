(ns bean-tests.provenance
  (:require [clojure.test :refer [deftest testing is run-all-tests]]
            [bean.provenance :as provenance]
            [bean.grid :as grid]))

(deftest provenance-test
  (testing "Provenance for simple expressions"
    (let [evaled-grid (grid/eval-sheet [["" "1" "=1+2" "=1+2+3"]])]
      (is (= (provenance/cell-proof [0 0] (:grid evaled-grid))
             {:address [0 0]
              :value nil
              :content ""
              :workings []}))
      (is (= (provenance/cell-proof [0 1] (:grid evaled-grid))
             {:address [0 1]
              :value 1
              :content "1"
              :workings {:self-evident true}}))
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             {:address [0 2]
              :value 3
              :content "=1+2"
              :workings [{:value 1
                          :workings {:self-evident true}}
                         {:value "+"
                          :workings {:self-evident true}}
                         {:value 2
                          :workings {:self-evident true}}]}))
      (is (= (provenance/cell-proof [0 3] (:grid evaled-grid))
             {:address [0 3]
              :value 6
              :content "=1+2+3"
              :workings [{:value 1
                          :workings {:self-evident true}}
                         {:value "+"
                          :workings {:self-evident true}}
                         {:value 5
                          :workings [{:value 2
                                      :workings {:self-evident true}}
                                     {:value "+"
                                      :workings {:self-evident true}}
                                     {:value 3
                                      :workings {:self-evident true}}]}]}))))

  (testing "Provenance for cell references"
    (let [evaled-grid (grid/eval-sheet [["1" "=A1" "=B1"]])]
      (is (= (provenance/cell-proof [0 1] (:grid evaled-grid))
             {:address [0 1]
              :content "=A1"
              :value 1
              :workings {:ref
                         {:address [0 0]
                          :content "1"
                          :value 1
                          :workings {:self-evident true}}}}))
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             {:address [0 2]
              :content "=B1"
              :value 1
              :workings {:ref
                         {:address [0 1]
                          :content "=A1"
                          :value 1
                          :workings {:ref
                                     {:address [0 0]
                                      :content "1"
                                      :value 1
                                      :workings {:self-evident true}}}}}}))))

  (testing "Provenance for multiple cell references"
    (let [evaled-grid (grid/eval-sheet [["1" "2" "=A1+B1"]
                                        ["=C1" "" ""]])]
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             {:address [0 2]
              :content "=A1+B1"
              :value 3
              :workings [{:value 1
                          :workings {:ref
                                     {:address [0 0]
                                      :content "1"
                                      :value 1
                                      :workings {:self-evident true}}}}
                         {:value "+"
                          :workings {:self-evident true}}
                         {:value 2
                          :workings {:ref
                                     {:address [0 1]
                                      :content "2"
                                      :value 2
                                      :workings {:self-evident true}}}}]}))
      (is (= (provenance/cell-proof [1 0] (:grid evaled-grid))
             {:address [1 0]
              :content "=C1"
              :value 3
              :workings {:ref
                         {:address [0 2]
                          :content "=A1+B1"
                          :value 3
                          :workings [{:value 1
                                      :workings {:ref
                                                 {:address [0 0]
                                                  :content "1"
                                                  :value 1
                                                  :workings {:self-evident true}}}}
                                     {:value "+"
                                      :workings {:self-evident true}}
                                     {:value 2
                                      :workings {:ref
                                                 {:address [0 1]
                                                  :content "2"
                                                  :value 2
                                                  :workings {:self-evident true}}}}]}}})))))

(run-all-tests)
