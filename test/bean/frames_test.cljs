(ns bean.frames-test
  (:require [bean.grid :as grid]
            [bean.frames :as frames]
            [clojure.test :refer [deftest testing is]]))

(defn- new-sheet []
  (grid/eval-sheet (grid/new-sheet (repeat 5 (repeat 5 "")) "")))

(deftest make-frame-test
  (testing "Creates a frame"
    (let [sheet (frames/make-frame (new-sheet) "A frame" {:start [0 0] :end [2 2]})]
      (is (= (frames/get-frame sheet "A frame") {:start [0 0] :end [2 2] :labels {} :skip-cells #{}})))))

(deftest cell-frame-test
  (testing "Gets a cell's frame name"
    (let [sheet (frames/make-frame (new-sheet) "A frame" {:start [0 0] :end [2 2]})]
      (is (= (frames/cell-frame [1 1] sheet) "A frame"))
      (is (= (frames/cell-frame [3 3] sheet) nil)))))

(deftest add-label-test
  (testing "Adds labels to a frame"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [1 1] :top)
                    (frames/add-label frame-name [1 2] :left 0x000000))]
      (is (= (get-in sheet [:frames frame-name :labels]) {[1 1] {:dirn :top :color nil}
                                                          [1 2] {:dirn :left :color 0x000000}})))))

(deftest blocking-label
  (testing "Top labels of the same direction block labels that exist above"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [3 3]})
                    (frames/add-label frame-name [0 0] :top)
                    (frames/add-label frame-name [2 0] :top))]
      (frames/blocking-label sheet frame-name [0 0])))

  (testing "Top labels of the same direction and span block labels that exist above"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [3 3]})
                    (frames/add-label frame-name [0 0] :top)
                    (grid/merge-cells {:start [0 0] :end [0 1]})
                    (frames/add-label frame-name [2 0] :top)
                    (grid/merge-cells {:start [2 0] :end [2 1]}))]
      (frames/blocking-label sheet frame-name [0 0]))))

(deftest label->cells-test
  (testing "Gets cells under a simple top label"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [1 1] :top))]
      (is (= (frames/label->cells sheet frame-name [1 1]) #{[2 1]}))))

  (testing "Gets cells under a simple left label"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [1 1] :left))]
      (is (= (frames/label->cells sheet frame-name [1 1]) #{[1 2]}))))

  (testing "Gets cells under a merged top label"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [1 1] :top)
                    (grid/merge-cells {:start [1 1] :end [1 2]}))]
      (is (= (frames/label->cells sheet frame-name [1 1]) #{[2 1] [2 2]}))))

  (testing "Doesn't include other labels in the result"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [0 0] :top)
                    (grid/merge-cells {:start [0 0] :end [0 1]})
                    (frames/add-label frame-name [1 0] :top))]
      (is (nil? (get [1 0] (frames/label->cells sheet frame-name [0 0]))))))

  (testing "Includes skip cells from the result"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [1 1] :top)
                    (frames/mark-skipped frame-name [[2 1]]))]
      (is (some? (get-in sheet [:frames frame-name :skip-cells [2 1]])))
      (is (some? (get (frames/label->cells sheet frame-name [1 1]) [2 1]))))))

(deftest skipped-cells-test
  (testing "Returns skipped cells and cells under a skip label"
    (let [frame-name "A frame"
          sheet (-> (new-sheet)
                    (frames/make-frame frame-name {:start [0 0] :end [2 2]})
                    (frames/add-label frame-name [1 1] :top)
                    (frames/mark-skipped frame-name [[1 1]]))]
      (is (some? (get (frames/skipped-cells sheet frame-name) [2 1]))))))
