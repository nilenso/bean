(ns bean-tests.provenance
  (:require [clojure.test :refer [deftest testing is]]
            [bean.provenance :as provenance]
            [bean.grid :as grid]))

(deftest provenance-test
  (testing "Provenance for simple expressions"
    (let [evaled-grid (grid/eval-sheet [["" "1" "=1+2" "=1+2+3"]])]
      (is (= (provenance/cell-proof [0 0] (:grid evaled-grid))
             {:address [0 0]
              :value nil
              :content ""
              :workings nil}))
      (is (= (provenance/cell-proof [0 1] (:grid evaled-grid))
             {:address [0 1]
              :value 1
              :content "1"
              :workings {:self-evident 1}}))
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             {:address [0 2]
              :value 3
              :content "=1+2"
              :workings {:value 3
                         :workings [{:self-evident 1}
                                    {:self-evident "+"}
                                    {:self-evident 2}]}}))
      (is (= (provenance/cell-proof [0 3] (:grid evaled-grid))
             {:address [0 3]
              :value 6
              :content "=1+2+3"
              :workings {:value 6
                         :workings [{:self-evident 1}
                                    {:self-evident "+"}
                                    {:value 5
                                     :workings [{:self-evident 2}
                                                {:self-evident "+"}
                                                {:self-evident 3}]}]}})))))
