(ns bean.ui.main
  (:require [reagent.dom :as r]
            [reagent.core :as rc]
            [bean.grid :as grid]
            [bean.ui.sheet :as sheet]))

(def sheet (rc/atom nil))
(def selected-cell (rc/atom nil))

(defn- start-sheet []
  (vec
   (for [row-num (range 300)]
     (vec (map (fn [_] "") (range 12))))))

(defn update-cell [address content]
  (swap! sheet #(grid/eval-sheet % address content)))

(defn cell-selector []
  (when @selected-cell
   [:div
    {:id :cell-selector
     :style {:top (str (+ 30 (* (first @selected-cell) 30)) "px")
             :left (str (+ 40 (* (second @selected-cell) 110)) "px")
             :display :block}}]))

(defn set-mode [[r c] mode]
  (swap! sheet #(update-in % [:grid r c :mode] (constantly mode)))
  (when (= mode :edit) (reset! selected-cell [r c])))

(defn active-sheet []
  [sheet/sheet1 @sheet {:update-cell update-cell
                        :set-mode set-mode
                        :edit-mode #(set-mode % :edit)}])

#_(defn ^:export main []
  (reset! sheet {:grid (doall (for [i (range 200)]
                                [{:content "", :ast [:CellContents], :value nil, :representation "cell1"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}
                                 {:content "", :ast [:CellContents], :value nil, :representation "cell2"}]))})
  (r/render
   [:div
    [:div {:class :container}
     [cell-selector]
     [active-sheet]]]
   (.getElementById js/document "app")))


#_(defn ^:export main []
  (r/render
   [:div {:id :container
          :style {:display :grid 
                  "grid-template-columns" (apply str (for [i (range 100)] (str (+ 50 (rand-int 100)) "px ")))
                  :grid-template-rows "40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px 40px"}}
    [:div {:id "selector"}]
    ;; 50000 takes 0.5s
    ;; 100000 takes 1.2s
    (for [i (range 50000)]
      [:div 
       {:style {:overflow :hidden
                :border "0.25px solid #ddd"
                :padding "5px"
                :grid-column-start (when (= 1 (rand-int 20))
                                     (str "span " (rand-int 10)))
                :grid-row-start (when (= 1 (rand-int 20))
                                  "span 3")
                :background-color (when (= 1 (rand-int 50))
                                    "beige")}}])]
   (.getElementById js/document "app")))

(defn ^:export main []
    (r/render
     [:div {:id :container
            :style {:position :relative}}
      [:div {:id "selector"}]
      ;; 100000 takes 1.3s (without merge etc)
      (for [i (range 100000)]
        [:div
         {:style {:position "absolute"
                  :border "0.25px solid #ddd"
                  :padding "5px"
                  :top (str (* (/ i 100) 50) "px")
                  :left (str (* (mod i 100) 100) "px")
                  :background-color (when (= 1 (rand-int 50))
                                      "beige")}} "HAHAHAHAHHA"])]
     (.getElementById js/document "app")))
