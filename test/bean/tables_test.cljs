(ns bean.tables-test
  (:require [bean.grid :as grid]
            [bean.tables :as tables]
            [clojure.test :refer [deftest testing is]]))

(defn- new-sheet []
  (grid/eval-sheet (grid/new-sheet (repeat 5 (repeat 5 "")) "")))

(deftest make-table-test
  (testing "Creates a table"
    (let [sheet (tables/make-table (new-sheet) "A Table" {:start [0 0] :end [2 2]})]
      (is (= (tables/get-table sheet "A Table") {:start [0 0] :end [2 2] :labels {} :skip-cells #{}})))))

(deftest cell-table-test
  (testing "Gets a cell's table name"
    (let [sheet (tables/make-table (new-sheet) "A Table" {:start [0 0] :end [2 2]})]
      (is (= (tables/cell-table [1 1] sheet) "A Table"))
      (is (= (tables/cell-table [3 3] sheet) nil)))))

(deftest add-label-test
  (testing "Adds labels to a table"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :top)
                    (tables/add-label table-name [1 2] :left 0x000000))]
      (is (= (get-in sheet [:tables table-name :labels]) {[1 1] {:dirn :top :color nil}
                                                          [1 2] {:dirn :left :color 0x000000}})))))

(deftest blocking-label
  (testing "Top labels of the same direction block labels that exist above"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [3 3]})
                    (tables/add-label table-name [0 0] :top)
                    (tables/add-label table-name [2 0] :top))]
      (tables/blocking-label sheet table-name [0 0])))

  (testing "Top labels of the same direction and span block labels that exist above"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [3 3]})
                    (tables/add-label table-name [0 0] :top)
                    (grid/merge-cells {:start [0 0] :end [0 1]})
                    (tables/add-label table-name [2 0] :top)
                    (grid/merge-cells {:start [2 0] :end [2 1]}))]
      (tables/blocking-label sheet table-name [0 0]))))

(deftest label->cells-test
  (testing "Gets cells under a simple top label"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :top))]
      (is (= (tables/label->cells sheet table-name [1 1]) #{[2 1]}))))

  (testing "Gets cells under a simple left label"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :left))]
      (is (= (tables/label->cells sheet table-name [1 1]) #{[1 2]}))))

  (testing "Gets cells under a merged top label"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :top)
                    (grid/merge-cells {:start [1 1] :end [1 2]}))]
      (is (= (tables/label->cells sheet table-name [1 1]) #{[2 1] [2 2]}))))

  (testing "Doesn't include other labels in the result"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [0 0] :top)
                    (grid/merge-cells {:start [0 0] :end [0 1]})
                    (tables/add-label table-name [1 0] :top))]
      (is (nil? (get [1 0] (tables/label->cells sheet table-name [0 0]))))))

  (testing "Includes skip cells from the result"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :top)
                    (tables/mark-skipped table-name [[2 1]]))]
      (is (some? (get-in sheet [:tables table-name :skip-cells [2 1]])))
      (is (some? (get (tables/label->cells sheet table-name [1 1]) [2 1]))))))

(deftest skipped-cells-test
  (testing "Returns skipped cells and cells under a skip label"
    (let [table-name "A Table"
          sheet (-> (new-sheet)
                    (tables/make-table table-name {:start [0 0] :end [2 2]})
                    (tables/add-label table-name [1 1] :top)
                    (tables/mark-skipped table-name [[1 1]]))]
      (is (some? (get (tables/skipped-cells sheet table-name) [2 1]))))))
