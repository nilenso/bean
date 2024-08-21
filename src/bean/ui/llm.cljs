(ns bean.ui.llm 
  (:require [bean.util :as util]
            [clojure.set :as set]
            [clojure.string :as string]
            [hickory.render :as hr]
            [cljs.reader :refer [read-string]]))

(def system-prompt
  "You are annotating different parts of a table structure using this ruleset.\n- A label is any cell that categorises, points to or indexes data in the table \n(these can be headers, section headers, row headers, summary headers or other descriptors).\n- Labels have a \"direction\":\n    t2b (top-to-bottom) if the data is below it, \n    l2r (left-to-right) if the data is on it's right,\n    tl2br (top-left-to-bottom-right) if the data is diagonally aligned. \ntl2br labels point to data on their bottom right but don't span the entire range they point to. tl2br labels don't have a span.\n- Summaries, aggregates (sums/averages etc), notes and marginalia are all considered 'skipped' cells.\n\nExample 1\n<table><tbody><tr><td data-addr=\"[21 1]\"></td><td data-addr=\"[21 2]\">2020</td><td data-addr=\"[21 3]\" >2021</td><td data-addr=\"[21 4]\" >2022</td></tr><tr><td data-addr=\"[22 1]\" >Revenue</td><td data-addr=\"[22 2]\">74801</td><td data-addr=\"[22 3]\">98901</td><td data-addr=\"[22 4]\">69080</td></tr><tr><td data-addr=\"[23 1]\" >Expenses</td><td data-addr=\"[23 2]\">54522</td><td data-addr=\"[23 3]\">42024</td><td data-addr=\"[23 4]\">31429</td></tr><tr><td data-addr=\"[24 1]\" >Profit</td><td data-addr=\"[24 2]\"></td><td data-addr=\"[24 3]\"></td><td data-addr=\"[24 4]\"></td></tr></tbody></table>\nResponse:\n[21 2] [21 3] [21 4] are t2b labels\n[22 1] [23 1] [24 1] are l2r labels\n<labels>{[21 2] :t2b [21 3] :t2b [21 4] :t2b [22 1] :l2r [23 1] :l2r  [24 1] :l2r}</labels>\n<skips>#{}</skips>\n\nExample 2\n<table><tbody><tr><td data-addr=\"[3 1]\">Employee Data</td><td data-addr=\"[3 2]\" style=\"font-weight: bold;\">Name</td><td data-addr=\"[3 3]\" style=\"font-weight: bold;\">Country</td><td data-addr=\"[3 4]\" style=\"font-weight: bold;\">Salary</td><td data-addr=\"[3 5]\" style=\"font-weight: bold;\">Tax Amount</td></tr><tr><td colspan=\"1\" data-addr=\"[4 1]\" rowspan=\"3\" style=\"font-weight: bold;\">Engineering</td><td data-addr=\"[4 2]\">John</td><td data-addr=\"[4 3]\">Singapore</td><td data-addr=\"[4 4]\">24498</td><td data-addr=\"[4 5]\"></td></tr><tr><td data-addr=\"[5 2]\">Tanmay</td><td data-addr=\"[5 3]\">India</td><td data-addr=\"[5 4]\">10082</td><td data-addr=\"[5 5]\"></td></tr><tr><td data-addr=\"[6 2]\">James</td><td data-addr=\"[6 3]\">Finland</td><td data-addr=\"[6 4]\">78217</td><td data-addr=\"[6 5]\"></td></tr><tr><td data-addr=\"[7 1]\"></td><td data-addr=\"[7 2]\"></td><td data-addr=\"[7 3]\" style=\"font-weight: bold;\">Total</td><td data-addr=\"[7 4]\">13424</td><td data-addr=\"[7 5]\"></td></tr><tr><td colspan=\"1\" data-addr=\"[8 1]\" rowspan=\"2\" style=\"font-weight: bold;\">Sales</td><td data-addr=\"[8 2]\">Sujan</td><td data-addr=\"[8 3]\">India</td><td data-addr=\"[8 4]\">26560</td><td data-addr=\"[8 5]\"></td></tr><tr><td data-addr=\"[9 2]\">Samson</td><td data-addr=\"[9 3]\">Finland</td><td data-addr=\"[9 4]\">50393</td><td data-addr=\"[9 5]\"></td></tr><tr><td data-addr=\"[10 1]\"></td><td data-addr=\"[10 2]\"></td><td data-addr=\"[10 3]\"></td><td data-addr=\"[10 4]\"></td><td data-addr=\"[10 5]\"></td></tr><tr><td data-addr=\"[11 1]\"></td><td data-addr=\"[11 2]\"></td><td data-addr=\"[11 3]\"></td><td data-addr=\"[11 4]\"></td><td data-addr=\"[11 5]\"></td></tr><tr><td data-addr=\"[12 1]\"></td><td data-addr=\"[12 2]\"></td><td data-addr=\"[12 3]\">Note</td><td data-addr=\"[12 4]\">Salary is Pre-Tax</td><td data-addr=\"[12 5]\"></td></tr></tbody></table>                \nResponse:\n[3 2] [3 3] [3 4] [3 5] are t2b labels\n[4 1] [7 3] [8 1]  are l2r labels\n[3 1] is a tl2br label since it points to the entire table\n[12 3] [7 3] are skip cells\n<labels>{[4 1] :l2r [3 2] :t2b [3 3] :t2b [3 4] :t2b [7 1] :l2r [7 3] :l2r [3 1] :tl2br}</labels>\n<skips>#{[12 3] [7 3]}</skips>\n\nExample 3\n<table><tbody><tr><td data-addr=\"[47 8]\"></td><td data-addr=\"[47 9]\">City</td><td data-addr=\"[47 10]\" style=\"font-weight: bold;\">Population</td></tr><tr><td data-addr=\"[48 8]\" style=\"font-weight: bold;\">South</td><td data-addr=\"[48 9]\">Bangalore</td><td data-addr=\"[48 10]\">1200</td></tr><tr><td data-addr=\"[49 8]\"></td><td data-addr=\"[49 9]\">Pondicherry</td><td data-addr=\"[49 10]\">950000</td></tr><tr><td data-addr=\"[50 8]\"></td><td data-addr=\"[50 9]\">Kochi</td><td data-addr=\"[50 10]\">2100000</td></tr><tr><td data-addr=\"[51 8]\"></td><td data-addr=\"[51 9]\">Chennai</td><td data-addr=\"[51 10]\">7100000</td></tr><tr><td data-addr=\"[52 8]\" style=\"font-weight: bold;\">North</td><td data-addr=\"[52 9]\">Mumbai</td><td data-addr=\"[52 10]\">12500000</td></tr><tr><td data-addr=\"[53 8]\"></td><td data-addr=\"[53 9]\">Delhi</td><td data-addr=\"[53 10]\">18900000</td></tr><tr><td data-addr=\"[54 8]\"></td><td data-addr=\"[54 9]\">Chandigarh</td><td data-addr=\"[54 10]\">1100000</td></tr></tbody></table>\nResponse:\n[47 9] [47 10] are t2b labels\n[48 8] [52 8] are tl2br labels\n<labels>{[47 9] :t2b [47 10] :t2b [48 8] :tl2br [52 8] :tl2br}</addresses>\n<skips>#{}</skips>\n\nExample 4         \n<table><tbody><tr><td data-addr=\\\"[61 6]\\\">Revenue</td><td data-addr=\\\"[61 7]\\\">Expenses</td><td data-addr=\\\"[61 8]\\\">Profit</td></tr><tr><td colspan=\\\"3\\\" data-addr=\\\"[62 6]\\\" rowspan=\\\"1\\\" style=\\\";\\\">Q1</td></tr><tr><td data-addr=\\\"[63 6]\\\">1314</td><td data-addr=\\\"[63 7]\\\">42</td><td data-addr=\\\"[63 8]\\\">1272</td></tr><tr><td data-addr=\\\"[64 6]\\\">1314</td><td data-addr=\\\"[64 7]\\\">42</td><td data-addr=\\\"[64 8]\\\">1272</td></tr><tr><td data-addr=\\\"[65 6]\\\">1314</td><td data-addr=\\\"[65 7]\\\">42</td><td data-addr=\\\"[65 8]\\\">1272</td></tr><tr><td data-addr=\\\"[66 6]\\\">1314</td><td data-addr=\\\"[66 7]\\\">42</td><td data-addr=\\\"[66 8]\\\">1272</td></tr><tr><td data-addr=\\\"[67 6]\\\">Average</td><td data-addr=\\\"[67 7]\\\"></td><td data-addr=\\\"[67 8]\\\">5088</td></tr><tr><td colspan=\\\"3\\\" data-addr=\\\"[68 6]\\\" rowspan=\\\"1\\\" style=\\\";\\\">Q2</td></tr><tr><td data-addr=\\\"[69 6]\\\">1314</td><td data-addr=\\\"[69 7]\\\">42</td><td data-addr=\\\"[69 8]\\\">1272</td></tr><tr><td data-addr=\\\"[70 6]\\\">1314</td><td data-addr=\\\"[70 7]\\\">42</td><td data-addr=\\\"[70 8]\\\">1272</td></tr><tr><td data-addr=\\\"[71 6]\\\">1314</td><td data-addr=\\\"[71 7]\\\">42</td><td data-addr=\\\"[71 8]\\\">1272</td></tr><tr><td data-addr=\\\"[72 6]\\\">1314</td><td data-addr=\\\"[72 7]\\\">42</td><td data-addr=\\\"[72 8]\\\">1272</td></tr><tr><td data-addr=\\\"[73 6]\\\">Average</td><td data-addr=\\\"[73 7]\\\"></td><td data-addr=\\\"[73 8]\\\">5088</td></tr></tbody></table>\nResponse:\n[61 6] [61 7] [61 8] [62 6] [68 6] are t2b labels\n[67 6] [73 6] are l2r labels\n[67 6] [73 6] are skip labels\n<labels>{[61 6] :t2b [61 7] :t2b [61 8] :t2b [62 6] :t2b [68 6] :t2b}</addresses>\n<skips>#{[67 6] [73 6]}</skips>\n\nFind the labels in the given table. You don't care about blank cells. First explain your reasoning in <thinking><thinking/>and then include only the addresses and their directions in <labels></labels> and skipped cells in <skip-cells>."
)

(defn call-llm-server [prompt]
  (-> (js/fetch "http://localhost:8000"
                (clj->js
                 {:method "POST" 
                  :body (js/JSON.stringify
                         #js {:user_prompt prompt
                              :system_prompt system-prompt})}))
      (.then #(.text %))))

(defn hiccup-matrix->html [matrix]
  (hr/hiccup-to-html
   [[:table {}
     (into [:tbody {}] matrix)]]))

(defn merged-with-another? [cell]
  (and (get-in cell [:style :merged-with])
       (not (get-in cell [:style :merged-until]))))

(defn cell->hiccup-cell [cell [r c]]
  (let [[mr mc] (get-in cell [:style :merged-until])]
    (when-not (merged-with-another? cell)
      [:td
       (merge
        {:data-addr [r c]}
        (when-not (empty? (:style cell))
          {:style (string/join ";"
                               [(when (get-in cell [:style :bold])
                                  "font-weight: bold")
                                (when-let [bg (get-in cell [:style :background])]
                                  (str "background: " (.toString bg 16)))])})
        (when mc {:colspan (str (inc (- mc c)))})
        (when mr {:rowspan (str (inc (- mr r)))}))
       (:representation cell)])))

(defn frame->html
  [{:keys [start end]} sheet]
  (->> (util/addresses-matrix start end)
       (util/map-on-matrix #(cell->hiccup-cell (get-in (:grid sheet) %) %))
       (map #(into [] (remove nil? (into [:tr {}] %))))
       hiccup-matrix->html))

(def llm-dirn->bean-dirn
  {:t2b :top
   :l2r :left
   :tl2br :top-left})

(defn extract-tag-from-text [tag s]
  (second (re-find (re-pattern (str "(?s)<" tag ">(.*?)</" tag ">")) s)))

(defn suggest-labels [sheet frame-name]
  (let [frame (get-in sheet [:frames frame-name])
        frame-html (frame->html frame sheet)
        prompt frame-html]
    (-> (call-llm-server prompt)
        (.then
         #(let [label-dirns (->> (extract-tag-from-text "labels" %)
                                 read-string
                                 (reduce (fn [mapped-dirns [k v]]
                                           (assoc mapped-dirns k (get llm-dirn->bean-dirn v)))
                                         {})
                                ;;  (filter (fn [[k v]] (if (seq v) [k v] [])))
                                 )
                skip-cells (-> (extract-tag-from-text "skip-cells" %) read-string)
                ;; existing-labels (keys (:labels frame))
                ;; existing-skip-cells (keys (:skip-cells frame))
                ;; new-labels (apply dissoc label-dirns existing-labels)
                ;; new-skip-cells (set/difference skip-cells (set existing-skip-cells))
                ]
            {:frame-name frame-name
             :labels label-dirns
             :skip-cells skip-cells
             :explanation %})))))


