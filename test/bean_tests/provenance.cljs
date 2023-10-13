(ns bean-tests.provenance
  (:require [clojure.test :refer [deftest testing is run-all-tests]]
            [bean.provenance :as provenance]
            [bean.grid :as grid]))

(deftest provenance-test
  (testing "Provenance for simple expressions"
    (let [evaled-grid (grid/eval-sheet [["" "1" "=1+2" "=1+2+3"]])]
      (is (= (provenance/cell-proof [0 0] (:grid evaled-grid))
             [:cell-ref
              {:address [0 0] :value nil :content ""}
              []]))
      (is (= (provenance/cell-proof [0 1] (:grid evaled-grid))
             [:cell-ref
              {:address [0 1] :value 1 :content "1"}
              [:value 1 :self-evident]]))
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             [:cell-ref
              {:address [0 2] :value 3 :content "=1+2"}
              [:value 3
               [:value 1 :self-evident]
               [:value "+" :self-evident]
               [:value 2 :self-evident]]]))
      (is (= (provenance/cell-proof [0 3] (:grid evaled-grid))
             [:cell-ref
              {:address [0 3] :value 6 :content "=1+2+3"}
              [:value 6
               [:value 1 :self-evident]
               [:value "+" :self-evident]
               [:value 5
                [:value 2 :self-evident]
                [:value "+" :self-evident]
                [:value 3 :self-evident]]]]))))

  (testing "Provenance for cell references"
    (let [evaled-grid (grid/eval-sheet [["1" "=A1" "=B1"]])]
      (is (= (provenance/cell-proof [0 1] (:grid evaled-grid))
             [:cell-ref
              {:address [0 1] :content "=A1" :value 1}
              [:cell-ref
               {:address [0 0] :content "1" :value 1}
               [:value 1 :self-evident]]]))
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             [:cell-ref
              {:address [0 2] :content "=B1" :value 1}
              [:cell-ref
               {:address [0 1] :content "=A1" :value 1}
               [:cell-ref
                {:address [0 0] :content "1" :value 1}
                [:value 1 :self-evident]]]]))))

  (testing "Provenance for multiple cell references"
    (let [evaled-grid (grid/eval-sheet [["1" "2" "=A1+B1"]
                                        ["=C1" "" ""]])]
      (is (= (provenance/cell-proof [0 2] (:grid evaled-grid))
             [:cell-ref
              {:address [0 2] :content "=A1+B1" :value 3}
              [:value 3
               [:cell-ref
                {:address [0 0] :content "1" :value 1}
                [:value 1 :self-evident]]
               [:value "+" :self-evident]
               [:cell-ref
                {:address [0 1] :content "2" :value 2}
                [:value 2 :self-evident]]]]))
      (is (= (provenance/cell-proof [1 0] (:grid evaled-grid))
             [:cell-ref
              {:address [1 0] :content "=C1" :value 3}
              [:cell-ref
               {:address [0 2] :content "=A1+B1" :value 3}
               [:value 3
                [:cell-ref
                 {:address [0 0] :content "1" :value 1}
                 [:value 1 :self-evident]]
                [:value "+" :self-evident]
                [:cell-ref
                 {:address [0 1] :content "2" :value 2}
                 [:value 2 :self-evident]]]]]))))

  (testing "Provenance of spilled cells"
    (let [evaled-grid (grid/eval-sheet [["1" "=A1:A2" ""]
                                        ["2" "" ""]])]
      (is (= (provenance/cell-proof [0 1] (:grid evaled-grid))
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2"
               :value 1
               :relative-address [0 0]}
              [:cell-ref
               {:address [0 0] :content "1" :value 1}
               [:value 1 :self-evident]]]))
      (is (= (provenance/cell-proof [1 1] (:grid evaled-grid))
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2"
               :value 2
               :relative-address [1 0]}
              [:cell-ref
               {:address [1 0] :content "2" :value 2}
               [:value 2 :self-evident]]])))))

(run-all-tests)
