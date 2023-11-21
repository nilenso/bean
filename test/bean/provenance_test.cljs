(ns bean.provenance-test
  (:require [clojure.test :refer [deftest testing is run-all-tests]]
            [bean.provenance :as provenance]
            [bean.grid :as grid]))

(deftest provenance-test
  (testing "Provenance for simple expressions"
    (let [evaled-grid (grid/eval-sheet
                       (grid/new-sheet [["" "1" "=1+2" "=1+2+3"]] ""))]
      (is (= (provenance/cell-proof [0 0] evaled-grid)
             [:cell-ref
              {:address [0 0] :scalar nil :content ""}
              []]))
      (is (= (provenance/cell-proof [0 1] evaled-grid)
             [:cell-ref
              {:address [0 1] :scalar 1 :content "1"}
              [:scalar 1 :self-evident]]))
      (is (= (provenance/cell-proof [0 2] evaled-grid)
             [:cell-ref
              {:address [0 2] :scalar 3 :content "=1+2"}
              [:scalar 3
               [:scalar 1 :self-evident]
               [:scalar "+" :self-evident]
               [:scalar 2 :self-evident]]]))
      (is (= (provenance/cell-proof [0 3] evaled-grid)
             [:cell-ref
              {:address [0 3] :scalar 6 :content "=1+2+3"}
              [:scalar 6
               [:scalar 1 :self-evident]
               [:scalar "+" :self-evident]
               [:scalar 5
                [:scalar 2 :self-evident]
                [:scalar "+" :self-evident]
                [:scalar 3 :self-evident]]]]))))

  (testing "Provenance for cell references"
    (let [evaled-grid (grid/eval-sheet
                       (grid/new-sheet [["1" "=A1" "=B1"]] ""))]
      (is (= (provenance/cell-proof [0 1] evaled-grid)
             [:cell-ref
              {:address [0 1] :content "=A1" :scalar 1}
              [:cell-ref
               {:address [0 0] :content "1" :scalar 1}
               [:scalar 1 :self-evident]]]))
      (is (= (provenance/cell-proof [0 2] evaled-grid)
             [:cell-ref
              {:address [0 2] :content "=B1" :scalar 1}
              [:cell-ref
               {:address [0 1] :content "=A1" :scalar 1}
               [:cell-ref
                {:address [0 0] :content "1" :scalar 1}
                [:scalar 1 :self-evident]]]]))))

  (testing "Provenance for multiple cell references"
    (let [evaled-grid (grid/eval-sheet
                       (grid/new-sheet [["1" "2" "=A1+B1"] ["=C1" "" ""]] ""))]
      (is (= (provenance/cell-proof [0 2] evaled-grid)
             [:cell-ref
              {:address [0 2] :content "=A1+B1" :scalar 3}
              [:scalar 3
               [:cell-ref
                {:address [0 0] :content "1" :scalar 1}
                [:scalar 1 :self-evident]]
               [:scalar "+" :self-evident]
               [:cell-ref
                {:address [0 1] :content "2" :scalar 2}
                [:scalar 2 :self-evident]]]]))
      (is (= (provenance/cell-proof [1 0] evaled-grid)
             [:cell-ref
              {:address [1 0] :content "=C1" :scalar 3}
              [:cell-ref
               {:address [0 2] :content "=A1+B1" :scalar 3}
               [:scalar 3
                [:cell-ref
                 {:address [0 0] :content "1" :scalar 1}
                 [:scalar 1 :self-evident]]
                [:scalar "+" :self-evident]
                [:cell-ref
                 {:address [0 1] :content "2" :scalar 2}
                 [:scalar 2 :self-evident]]]]]))))

  (testing "Provenance of spilled cells"
    (let [evaled-grid (grid/eval-sheet
                       (grid/new-sheet [["1" "=A1:A2" ""] ["2" "" ""]] ""))]
      (is (= (provenance/cell-proof [0 1] evaled-grid)
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2"
               :address [0 1]
               :scalar 1
               :relative-address [0 0]}
              [:cell-ref
               {:address [0 0] :content "1" :scalar 1}
               [:scalar 1 :self-evident]]]))
      (is (= (provenance/cell-proof [1 1] evaled-grid)
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2"
               :address [1 1]
               :scalar 2
               :relative-address [1 0]}
              [:cell-ref
               {:address [1 0] :content "2" :scalar 2}
               [:scalar 2 :self-evident]]]))))

  (testing "Provenance for matrix opertions"
    (let [evaled-grid (grid/eval-sheet
                       (grid/new-sheet [["1" "=A1:A2+C1:C2" "3"]
                                        ["2" "" "=A1"]]
                                       ""))]
      (is (= (provenance/cell-proof [1 1] evaled-grid)
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2+C1:C2"
               :address [1 1]
               :scalar 3
               :relative-address [1 0]}
              [:scalar 3
               [:cell-ref
                {:address [1 0] :content "2" :scalar 2}
                [:scalar 2 :self-evident]]
               [:scalar "+" :self-evident]
               [:cell-ref
                {:address [1 2] :content "=A1" :scalar 1}
                [:cell-ref
                 {:address [0 0] :content "1" :scalar 1}
                 [:scalar 1 :self-evident]]]]]))))

  (testing "Provenance for matrix-scalar operations"
    (let [evaled-grid (grid/eval-sheet (grid/new-sheet [["1" "=A1:A2+1"]
                                                        ["2" ""]]
                                                       ""))]
      (is (= (provenance/cell-proof [1 1] evaled-grid)
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2+1"
               :address [1 1]
               :scalar 3
               :relative-address [1 0]}
              [:scalar 3
               [:cell-ref
                {:address [1 0] :content "2" :scalar 2}
                [:scalar 2 :self-evident]]
               [:scalar "+" :self-evident]
               [:scalar 1 :self-evident]]])))

    (let [evaled-grid (grid/eval-sheet
                       (grid/new-sheet [["1" "=A1:A2+C1:C2+D1:D2+1" "3" "0"]
                                        ["2" "" "4" "0"]]
                                       ""))]
      (is (= (provenance/cell-proof [1 1] evaled-grid)
             [:spill
              {:spilled-from [0 1]
               :content "=A1:A2+C1:C2+D1:D2+1"
               :address [1 1]
               :scalar 7
               :relative-address [1 0]}
              [:scalar 7
               [:cell-ref
                {:address [1 0] :content "2" :scalar 2}
                [:scalar 2 :self-evident]]
               [:scalar "+" :self-evident]
               [:scalar 5
                [:cell-ref
                 {:address [1 2] :content "4" :scalar 4}
                 [:scalar 4 :self-evident]]
                [:scalar "+" :self-evident]
                [:scalar 1
                 [:cell-ref
                  {:address [1 3] :content "0" :scalar 0}
                  [:scalar 0 :self-evident]]
                 [:scalar "+" :self-evident]
                 [:scalar 1 :self-evident]]]]])))))
