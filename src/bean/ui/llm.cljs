(ns bean.ui.llm
  (:require [bean.util :as util]
            [clojure.set :as set]
            [clojure.string :as string]
            [hickory.render :as hr]
            [cljs.reader :refer [read-string]]))

(def system-prompt
"You are annotating different parts of a table structure using this ruleset.\n- A label is any cell that categorises, points to or indexes data in the table \n(these can be headers, section headers, row headers, summary headers or other descriptors).\n- Labels have a \"direction\":\n    t2b (top-to-bottom) if the data is below it, \n    l2r (left-to-right) if the data is on it's right,\n    tl2br (top-left-to-bottom-right) if the data is diagonally aligned. \ntl2br labels point to data on their bottom right. tl2br labels don't span the entire range they point to. These need most attention while annotating.\n- Summaries, aggregates, units, text in parentheses/square brackets, notes and marginalia are all considered 'skipped' cells.\n\nExample 1\nShows a simple structure.\n<table><tbody><tr><td data-addr=\"[21 1]\"></td><td data-addr=\"[21 2]\">2020</td><td data-addr=\"[21 3]\" >2021</td><td data-addr=\"[21 4]\" >2022</td></tr><tr><td data-addr=\"[22 1]\" >Revenue</td><td data-addr=\"[22 2]\">74801</td><td data-addr=\"[22 3]\">98901</td><td data-addr=\"[22 4]\">69080</td></tr><tr><td data-addr=\"[23 1]\" >Expenses</td><td data-addr=\"[23 2]\">54522</td><td data-addr=\"[23 3]\">42024</td><td data-addr=\"[23 4]\">31429</td></tr><tr><td data-addr=\"[24 1]\" >Profit</td><td data-addr=\"[24 2]\"></td><td data-addr=\"[24 3]\"></td><td data-addr=\"[24 4]\"></td></tr></tbody></table>\nResponse:\n[21 2] [21 3] [21 4] are t2b labels\n[22 1] [23 1] [24 1] are l2r labels\n<labels>{[21 2] :t2b [21 3] :t2b [21 4] :t2b [22 1] :l2r [23 1] :l2r  [24 1] :l2r}</labels>\n<skips>#{}</skips>\n\nExample 2\nShows a table with summaries in between and a mixture of labels.\n<table><tbody><tr><td data-addr=\"[3 1]\">Employee Data</td><td data-addr=\"[3 2]\" style=\"font-weight: bold;\">Name</td><td data-addr=\"[3 3]\" style=\"font-weight: bold;\">Country</td><td data-addr=\"[3 4]\" style=\"font-weight: bold;\">Salary</td><td data-addr=\"[3 5]\" style=\"font-weight: bold;\">Tax Amount</td></tr><tr><td colspan=\"1\" data-addr=\"[4 1]\" rowspan=\"3\" style=\"font-weight: bold;\">Engineering</td><td data-addr=\"[4 2]\">John</td><td data-addr=\"[4 3]\">Singapore</td><td data-addr=\"[4 4]\">24498</td><td data-addr=\"[4 5]\"></td></tr><tr><td data-addr=\"[5 2]\">Tanmay</td><td data-addr=\"[5 3]\">India</td><td data-addr=\"[5 4]\">10082</td><td data-addr=\"[5 5]\"></td></tr><tr><td data-addr=\"[6 2]\">James</td><td data-addr=\"[6 3]\">Finland</td><td data-addr=\"[6 4]\">78217</td><td data-addr=\"[6 5]\"></td></tr><tr><td data-addr=\"[7 1]\"></td><td data-addr=\"[7 2]\"></td><td data-addr=\"[7 3]\" style=\"font-weight: bold;\">Total</td><td data-addr=\"[7 4]\">13424</td><td data-addr=\"[7 5]\"></td></tr><tr><td colspan=\"1\" data-addr=\"[8 1]\" rowspan=\"2\" style=\"font-weight: bold;\">Sales</td><td data-addr=\"[8 2]\">Sujan</td><td data-addr=\"[8 3]\">India</td><td data-addr=\"[8 4]\">26560</td><td data-addr=\"[8 5]\"></td></tr><tr><td data-addr=\"[9 2]\">Samson</td><td data-addr=\"[9 3]\">Finland</td><td data-addr=\"[9 4]\">50393</td><td data-addr=\"[9 5]\"></td></tr><tr><td data-addr=\"[10 1]\"></td><td data-addr=\"[10 2]\"></td><td data-addr=\"[10 3]\"></td><td data-addr=\"[10 4]\"></td><td data-addr=\"[10 5]\"></td></tr><tr><td data-addr=\"[11 1]\"></td><td data-addr=\"[11 2]\"></td><td data-addr=\"[11 3]\"></td><td data-addr=\"[11 4]\"></td><td data-addr=\"[11 5]\"></td></tr><tr><td data-addr=\"[12 1]\"></td><td data-addr=\"[12 2]\"></td><td data-addr=\"[12 3]\">Note</td><td data-addr=\"[12 4]\">Salary is Pre-Tax</td><td data-addr=\"[12 5]\"></td></tr></tbody></table>                \nResponse:\n[3 2] [3 3] [3 4] [3 5] are t2b labels\n[4 1] [7 3] [8 1]  are l2r labels\n[3 1] is a tl2br label since it points to the entire table but doesn't have a span\n[12 3] [7 3] are skip cells\n<labels>{[4 1] :l2r [3 2] :t2b [3 3] :t2b [3 4] :t2b [7 1] :l2r [7 3] :l2r [3 1] :tl2br}</labels>\n<skips>#{[12 3] [7 3]}</skips>\n\nExample 3\nAn example of top left labels used to create sections.\n<table><tbody><tr><td data-addr=\"[47 8]\"></td><td data-addr=\"[47 9]\">City</td><td data-addr=\"[47 10]\" style=\"font-weight: bold;\">Population</td></tr><tr><td data-addr=\"[48 8]\" style=\"font-weight: bold;\">South</td><td data-addr=\"[48 9]\">Bangalore</td><td data-addr=\"[48 10]\">1200</td></tr><tr><td data-addr=\"[49 8]\"></td><td data-addr=\"[49 9]\">Pondicherry</td><td data-addr=\"[49 10]\">950000</td></tr><tr><td data-addr=\"[50 8]\"></td><td data-addr=\"[50 9]\">Kochi</td><td data-addr=\"[50 10]\">2100000</td></tr><tr><td data-addr=\"[51 8]\"></td><td data-addr=\"[51 9]\">Chennai</td><td data-addr=\"[51 10]\">7100000</td></tr><tr><td data-addr=\"[52 8]\" style=\"font-weight: bold;\">North</td><td data-addr=\"[52 9]\">Mumbai</td><td data-addr=\"[52 10]\">12500000</td></tr><tr><td data-addr=\"[53 8]\"></td><td data-addr=\"[53 9]\">Delhi</td><td data-addr=\"[53 10]\">18900000</td></tr><tr><td data-addr=\"[54 8]\"></td><td data-addr=\"[54 9]\">Chandigarh</td><td data-addr=\"[54 10]\">1100000</td></tr></tbody></table>\nResponse:\n[47 9] [47 10] are t2b labels\n[48 8] [52 8] are tl2br labels\n<labels>{[47 9] :t2b [47 10] :t2b [48 8] :tl2br [52 8] :tl2br}</addresses>\n<skips>#{}</skips>\n\nExample 4\nAn example of labels that have the same direction and span creating sections (Q1 and Q2).\n<table><tbody><tr><td data-addr=\"[61 6]\">Revenue</td><td data-addr=\"[61 7]\">Expenses</td><td data-addr=\"[61 8]\">Profit</td></tr><tr><td colspan=\"3\" data-addr=\"[62 6]\" rowspan=\"1\" style=\";\">Q1</td></tr><tr><td data-addr=\"[63 6]\">1314</td><td data-addr=\"[63 7]\">42</td><td data-addr=\"[63 8]\">1272</td></tr><tr><td data-addr=\"[64 6]\">1314</td><td data-addr=\"[64 7]\">42</td><td data-addr=\"[64 8]\">1272</td></tr><tr><td data-addr=\"[65 6]\">1314</td><td data-addr=\"[65 7]\">42</td><td data-addr=\"[65 8]\">1272</td></tr><tr><td data-addr=\"[66 6]\">1314</td><td data-addr=\"[66 7]\">42</td><td data-addr=\"[66 8]\">1272</td></tr><tr><td colspan=\"3\" data-addr=\"[67 6]\" rowspan=\"1\" style=\";\">Q2</td></tr><tr><td data-addr=\"[68 6]\">1314</td><td data-addr=\"[68 7]\">42</td><td data-addr=\"[68 8]\">1272</td></tr><tr><td data-addr=\"[69 6]\">1314</td><td data-addr=\"[69 7]\">42</td><td data-addr=\"[69 8]\">1272</td></tr><tr><td data-addr=\"[70 6]\">1314</td><td data-addr=\"[70 7]\">42</td><td data-addr=\"[70 8]\">1272</td></tr><tr><td data-addr=\"[71 6]\">1314</td><td data-addr=\"[71 7]\">42</td><td data-addr=\"[71 8]\">1272</td></tr></tbody></table>\nResponse:\n[61 6] [61 7] [61 8] [62 6] [67 6] are t2b labels\n<labels>{[61 6] :t2b [61 7] :t2b [61 8] :t2b [62 6] :t2b [67 6] :t2b}</addresses>\n<skips>#{}</skips>\n\nExample 6\nAn example of serial number/units for a label that should to be skipped.\n<table><tbody><tr><td data-addr=\"[18 5]\" style=\"font-weight: bold;\">2002</td><td data-addr=\"[18 6]\" style=\"font-weight: bold;\">2006</td><td data-addr=\"[18 7]\" style=\"font-weight: bold;\">2010</td></tr><tr><td data-addr=\"[19 5]\" style=\"font-weight: bold;\">(1)</td><td data-addr=\"[19 6]\" style=\"font-weight: bold;\">(2)</td><td data-addr=\"[19 7]\" style=\"font-weight: bold;\">cm</td></tr><tr><td data-addr=\"[20 5]\"></td><td data-addr=\"[20 6]\"></td><td data-addr=\"[20 7]\"></td></tr><tr><td data-addr=\"[21 5]\">21.2%</td><td data-addr=\"[21 6]\">17.5%</td><td data-addr=\"[21 7]\">17.4%</td></tr></tbody></table>\n[18 5] [18 6] [18 7] are t2b labels\n[19 5] [19 6] [19 7] are skip cells\n[19 5] [19 6] [19 7] are not labels\n<labels>{[18 5] :t2b [18 6]  :t2b  [18 7] :t2b }</labels>\n<skips>#{[19 5] [19 6] [19 7]}</skips>\n\nExample 7\nAn example of tl2br label.\n<table><tbody><tr><td colspan=\"3\" data-addr=\"[38 6]\" rowspan=\"1\" style=\";\">Percent of employees covered</td><td data-addr=\"[38 9]\"></td></tr><tr><td data-addr=\"[39 6]\"></td><td colspan=\"2\" data-addr=\"[39 7]\" rowspan=\"1\" style=\";\">Own company stock</td><td data-addr=\"[39 9]\">21.2%</td></tr><tr><td data-addr=\"[40 6]\"></td><td colspan=\"2\" data-addr=\"[40 7]\" rowspan=\"1\" style=\";\">Stock options</td><td data-addr=\"[40 9]\">13.1%</td></tr></tbody></table>\n[38 6] is a tl2br label since it describes the section on it's bottom right but it's not a merged cell\n[39 7] [40 7] are t2b labels\n<labels>{[38 6] :tl2br [39 7] :l2r [40 7] :l2r}</labels>\n<skips>#{}</skips>\n\nExample 8\nAn example similar to example 4.\n<table><tbody><tr><td colspan=\"5\" data-addr=\"[10 1]\" rowspan=\"1\" style=\";\">Product: Conv-Conforming Fixed Rate 30 Year</td><td data-addr=\"[10 6]\"></td></tr><tr><td data-addr=\"[11 1]\">Rate</td><td data-addr=\"[11 2]\">15 Day</td><td data-addr=\"[11 3]\">30 Day</td><td data-addr=\"[11 4]\">45 Day</td><td data-addr=\"[11 5]\">60 Day</td><td data-addr=\"[11 6]\"></td></tr><tr><td data-addr=\"[12 1]\">4</td><td data-addr=\"[12 2]\">102.125</td><td data-addr=\"[12 3]\">102</td><td data-addr=\"[12 4]\">101.875</td><td data-addr=\"[12 5]\">101.5</td><td data-addr=\"[12 6]\"></td></tr><tr><td data-addr=\"[13 1]\">3.875</td><td data-addr=\"[13 2]\">101.297</td><td data-addr=\"[13 3]\">101.172</td><td data-addr=\"[13 4]\">101.047</td><td data-addr=\"[13 5]\">100.672</td><td data-addr=\"[13 6]\"></td></tr><tr><td data-addr=\"[14 1]\">Max</td><td data-addr=\"[14 2]\">102.125</td><td data-addr=\"[14 3]\">102</td><td data-addr=\"[14 4]\">101.875</td><td data-addr=\"[14 5]\">101.5</td><td data-addr=\"[14 6]\"></td></tr></tbody></table>\nResponse:\n[10 1] [11 1] [11 2] [11 3] [11 4] are t2b labels\n[14 1] is a l2r label\n[14 1] is also a skip label\nNo tl2br labels\n<labels>{[10 1] :t2b [11 1] :t2b [11 2] :t2b [11 3] :t2b [11 4] :t2b [14 1] :l2r}</addresses>\n<skips>#{[14 1]}</skips>\n\nExample 9\n<table><tbody><tr><td data-addr=\"[61 1]\">Date</td><td data-addr=\"[61 2]\">QLEIX</td><td data-addr=\"[61 3]\">QLENX</td><td data-addr=\"[61 4]\">Benchmark*</td></tr><tr><td data-addr=\"[62 1]\">July-13</td><td data-addr=\"[62 2]\">0.60%</td><td data-addr=\"[62 3]\">0.60%</td><td data-addr=\"[62 4]\">0.26%</td></tr><tr><td data-addr=\"[63 1]\">August-13</td><td data-addr=\"[63 2]\">-2.88%</td><td data-addr=\"[63 3]\">-2.88%</td><td data-addr=\"[63 4]\">-1.06%</td></tr><tr><td data-addr=\"[64 1]\">September-13</td><td data-addr=\"[64 2]\">4.50%</td><td data-addr=\"[64 3]\">4.50%</td><td data-addr=\"[64 4]\">2.48%</td></tr><tr><td data-addr=\"[65 1]\">October-13</td><td data-addr=\"[65 2]\">4.31%</td><td data-addr=\"[65 3]\">4.21%</td><td data-addr=\"[65 4]\">1.95%</td></tr><tr><td data-addr=\"[66 1]\">November-13</td><td data-addr=\"[66 2]\">2.82%</td><td data-addr=\"[66 3]\">2.82%</td><td data-addr=\"[66 4]\">0.89%</td></tr><tr><td data-addr=\"[67 1]\">December-13</td><td data-addr=\"[67 2]\">1.52%</td><td data-addr=\"[67 3]\">1.50%</td><td data-addr=\"[67 4]\">1.07%</td></tr><tr><td data-addr=\"[68 1]\">January-14</td><td data-addr=\"[68 2]\">-3.04%</td><td data-addr=\"[68 3]\">-3.04%</td><td data-addr=\"[68 4]\">-1.86%</td></tr></tbody></table>\nResponse:\nWhile users may often think of the leftmost column as the \"primary key\" or identifier for the data rows, in this case, the dates are not serving that purpose. \n<labels>{[61 2] :t2b [61 3] :t2b [61 4] :t2b}</labels>\n<skip-cells>#{}</skip-cells>\n\nFind the labels in the given table. You don't care about blank cells. First explain your reasoning in <thinking><thinking/>. At the end include only the addresses and their directions in <labels></labels> and skipped cells in <skip-cells>.")

(defn call-llm-server [prompt api-key]
  (-> (js/fetch "https://prabhanshu-claudeforwarder.web.val.run"
                (clj->js
                 {:method "POST"
                  :headers {"Content-Type" "application/json"
                            "x-api-key" api-key}
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
        prompt frame-html
        api-key (:anthropic-api-key sheet)]
    (-> (call-llm-server prompt api-key)
        (.then
         #(let [label-dirns (->> (extract-tag-from-text "labels" %)
                                 read-string
                                 (reduce (fn [mapped-dirns [k v]]
                                           (assoc mapped-dirns k (get llm-dirn->bean-dirn v)))
                                         {}))
                skip-cells (-> (extract-tag-from-text "skip-cells" %) read-string)]
            {:frame-name frame-name
             :labels label-dirns
             :skip-cells skip-cells
             :explanation %})))))


