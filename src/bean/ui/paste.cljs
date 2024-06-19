(ns bean.ui.paste
  (:require [bean.ui.util :as util]
            [clojure.string :as string]
            [hickory.core :as hickory]
            [hickory.render :as hr]
            [hickory.convert :as hc]
            [hickory.select :as hs]))

(def sample "<html xmlns:v=\"urn:schemas-microsoft-com:vml\"\nxmlns:o=\"urn:schemas-microsoft-com:office:office\"\nxmlns:x=\"urn:schemas-microsoft-com:office:excel\"\nxmlns=\"http://www.w3.org/TR/REC-html40\">\n\n<head>\n<meta http-equiv=Content-Type content=\"text/html; charset=utf-8\">\n<meta name=ProgId content=Excel.Sheet>\n<meta name=Generator content=\"Microsoft Excel 15\">\n<link id=Main-File rel=Main-File\nhref=\"file:////Users/prabhanshu/Library/Group%20Containers/UBF8T346G9.Office/TemporaryItems/msohtmlclip/clip.htm\">\n<link rel=File-List\nhref=\"file:////Users/prabhanshu/Library/Group%20Containers/UBF8T346G9.Office/TemporaryItems/msohtmlclip/clip_filelist.xml\">\n<style>\n<!--table\n\t{mso-displayed-decimal-separator:\"\\.\";\n\tmso-displayed-thousand-separator:\"\\,\";}\n@page\n\t{margin:.75in .7in .75in .7in;\n\tmso-header-margin:.3in;\n\tmso-footer-margin:.3in;}\ntr\n\t{mso-height-source:auto;}\ncol\n\t{mso-width-source:auto;}\nbr\n\t{mso-data-placement:same-cell;}\ntd\n\t{padding-top:1px;\n\tpadding-right:1px;\n\tpadding-left:1px;\n\tmso-ignore:padding;\n\tcolor:black;\n\tfont-size:10.0pt;\n\tfont-weight:400;\n\tfont-style:normal;\n\ttext-decoration:none;\n\tfont-family:Arial;\n\tmso-generic-font-family:auto;\n\tmso-font-charset:0;\n\tmso-number-format:General;\n\ttext-align:general;\n\tvertical-align:bottom;\n\tborder:none;\n\tmso-background-source:auto;\n\tmso-pattern:auto;\n\tmso-protection:locked visible;\n\twhite-space:nowrap;\n\tmso-rotate:0;}\n.xl65\n\t{font-weight:700;\n\tfont-family:Arial, sans-serif;\n\tmso-font-charset:0;}\n.xl66\n\t{color:#1155CC;\n\ttext-decoration:underline;\n\ttext-underline-style:single;\n\tfont-family:Arial, sans-serif;\n\tmso-font-charset:0;}\n.xl67\n\t{font-family:Arial, sans-serif;\n\tmso-font-charset:0;}\n-->\n</style>\n</head>\n\n<div lang=en dir=ltr></div>\n\n<body link=\"#1155CC\" vlink=\"#1155CC\">\n\n<table border=0 cellpadding=0 cellspacing=0 width=174 style='border-collapse:\n collapse;width:130pt'>\n<!--StartFragment-->\n <col width=87 span=2 style='width:65pt'>\n <tr height=17 style='height:13.0pt'>\n  <td height=17 class=xl65 width=87 style='height:13.0pt;width:65pt'>Season\n  name</td>\n  <td class=xl65 width=87 style='width:65pt'>Episodes</td>\n </tr>\n <tr height=17 style='height:13.0pt'>\n  <td height=17 class=xl65 style='height:13.0pt'>Last aired</td>\n  <td></td>\n </tr>\n <tr height=17 style='height:13.0pt'>\n  <td height=17 class=xl66 style='height:13.0pt;padding-bottom:0in;padding-top:\n  0in' scope=col><a\n  href=\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Indigo_League\"\n  title=\"Pokémon: Indigo League\">Indigo League</a></td>\n  <td class=xl67 align=right>82</td>\n </tr>\n <tr height=17 style='height:13.0pt'>\n  <td height=17 class=xl66 style='height:13.0pt;padding-bottom:0in;padding-top:\n  0in' scope=col><a\n  href=\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Adventures_in_the_Orange_Islands\"\n  title=\"Pokémon: Adventures in the Orange Islands\">Adventures in the Orange\n  Islands</a></td>\n  <td class=xl67 align=right>36</td>\n </tr>\n<!--EndFragment-->\n</table>\n\n</body>\n\n</html>\n")
(def sample2 "<table class=\\\"wikitable plainrowheaders\\\" style=\\\"text-align:center;height:1px;display:table\\\"><tbody><tr style=\\\"height:100%\\\"><td colspan=\\\"2\\\"></td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">January 28, 1999</td><td style=\\\"padding:0 8px\\\">October 7, 1999</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#FFD700;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">3</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_The_Johto_Journeys\\\" title=\\\"Pokémon: The Johto Journeys\\\">The Johto Journeys</a></i></td><td colspan=\\\"2\\\">41</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">October 14, 1999</td><td style=\\\"padding:0 8px\\\">July 27, 2000</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#C0C0C0;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">4</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Johto_League_Champions\\\" title=\\\"Pokémon: Johto League Champions\\\">Johto League Champions</a></i></td><td colspan=\\\"2\\\">52</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">August 3, 2000</td><td style=\\\"padding:0 8px\\\">August 2, 2001</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#A7D8DE;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">5</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Master_Quest\\\" title=\\\"Pokémon: Master Quest\\\">Master Quest</a></i></td><td colspan=\\\"2\\\">65</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">August 9, 2001</td><td style=\\\"padding:0 8px\\\">November 14, 2002</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#9B111E;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">6</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Advanced\\\" title=\\\"Pokémon: Advanced\\\">Advanced</a></i></td><td colspan=\\\"2\\\">40</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">November 21, 2002</td><td style=\\\"padding:0 8px\\\">August 28, 2003</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#0F52BA;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">7</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Advanced_Challenge\\\" title=\\\"Pokémon: Advanced Challenge\\\">Advanced Challenge</a></i></td><td colspan=\\\"2\\\">52</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">September 4, 2003</td><td style=\\\"padding:0 8px\\\">September 2, 2004</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#50C878;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">8</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Advanced_Battle\\\" title=\\\"Pokémon: Advanced Battle\\\">Advanced Battle</a></i></td><td colspan=\\\"2\\\">53</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">September 9, 2004</td><td style=\\\"padding:0 8px\\\">September 29, 2005</td></tr><tr style=\\\"height:100%\\\"><th scope=\\\"row\\\" colspan=\\\"1\\\" style=\\\"height:inherit;padding:0\\\"><span style=\\\"text-align:center;float:left;width:100%;height:100%\\\"><span style=\\\"width:14px;background:#00DD00;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\\\"></span><span style=\\\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\\\"><span class=\\\"nowrap\\\">9</span></span></span></th><td scope=\\\"col\\\" rowspan=\\\"1\\\" style=\\\"padding:0 8px\\\"><i><a href=\\\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Battle_Frontier\\\" title=\\\"Pokémon: Battle Frontier\\\">Battle Frontier</a></i></td><td colspan=\\\"2\\\">47</td><td colspan=\\\"1\\\" style=\\\"padding:0.2em 0.4em\\\">October 6, 2005</td><td style=\\\"padding:0 8px\\\">Septembe</td></tr></tbody></table>")
(def sample3 "<html xmlns:v=\"urn:schemas-microsoft-com:vml\"xmlns:o=\"urn:schemas-microsoft-com:office:office\"xmlns:x=\"urn:schemas-microsoft-com:office:excel\"xmlns=\"http://www.w3.org/TR/REC-html40\"><head><meta http-equiv=Content-Type content=\"text/html; charset=utf-8\"><meta name=ProgId content=Excel.Sheet><meta name=Generator content=\"Microsoft Excel 15\"><link id=Main-File rel=Main-Filehref=\"file:////Users/prabhanshu/Library/Group%20Containers/UBF8T346G9.Office/TemporaryItems/msohtmlclip/clip.htm\"><link rel=File-Listhref=\"file:////Users/prabhanshu/Library/Group%20Containers/UBF8T346G9.Office/TemporaryItems/msohtmlclip/clip_filelist.xml\"><style><!--table	{}@page	{margin:.75in .7in .75in .7in;	mso-header-margin:.3in;	mso-footer-margin:.3in;}tr	{mso-height-source:auto;}col	{mso-width-source:auto;}br	{mso-data-placement:same-cell;}td	{padding-top:1px;	padding-right:1px;	padding-left:1px;	mso-ignore:padding;	color:black;	font-size:10.0pt;	font-weight:400;	font-style:normal;	text-decoration:none;	font-family:Arial;	mso-generic-font-family:auto;	mso-font-charset:0;	mso-number-format:General;	text-align:general;	vertical-align:bottom;	border:none;	mso-background-source:auto;	mso-pattern:auto;	mso-protection:locked visible;	white-space:nowrap;	mso-rotate:0;}.xl65	{font-family:Arial, sans-serif;	mso-font-charset:0;}.xl66	{font-weight:700;	font-family:Arial, sans-serif;	mso-font-charset:0;	text-align:center;}--></style></head><div lang=en dir=ltr></div><body link=\"#1155CC\" vlink=\"#1155CC\"><table border=0 cellpadding=0 cellspacing=0 width=174 style='border-collapse: collapse;width:130pt'><!--StartFragment--> <col width=87 span=2 style='width:65pt'> <tr height=17 style='height:13.0pt'>  <td colspan=2 height=17 class=xl66 width=174 style='height:13.0pt;width:130pt;  padding-bottom:0in;padding-top:0in'>fwefwe</td> </tr> <tr height=17 style='height:13.0pt'>  <td height=17 class=xl65 style='height:13.0pt;padding-bottom:0in;padding-top:  0in'>fwefwef</td>  <td></td> </tr> <tr height=17 style='height:13.0pt'>  <td height=17 style='height:13.0pt;padding-bottom:0in;padding-top:0in'></td>  <td class=xl65>fwefwe</td> </tr><!--EndFragment--></table></body></html>")
(def sample4 "<table class=\"wikitable plainrowheaders\" style=\"text-align:center;height:1px;display:table\"><tbody><tr style=\"height:100%\"><td colspan=\"1\" style=\"padding:0.2em 0.4em\">September 4, 2003</td><td style=\"padding:0 8px\">September 2, 2004</td></tr><tr style=\"height:100%\"><th scope=\"row\" colspan=\"1\" style=\"height:inherit;padding:0\"><span style=\"text-align:center;float:left;width:100%;height:100%\"><span style=\"width:14px;background:#50C878;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\"></span><span style=\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\"><span class=\"nowrap\">8</span></span></span></th><td scope=\"col\" rowspan=\"1\" style=\"padding:0 8px\"><i><a href=\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Advanced_Battle\" title=\"Pokémon: Advanced Battle\">Advanced Battle</a></i></td><td colspan=\"2\">53</td><td colspan=\"1\" style=\"padding:0.2em 0.4em\">September 9, 2004</td><td style=\"padding:0 8px\">September 29, 2005</td></tr><tr style=\"height:100%\"><th scope=\"row\" colspan=\"1\" style=\"height:inherit;padding:0\"><span style=\"text-align:center;float:left;width:100%;height:100%\"><span style=\"width:14px;background:#00DD00;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\"></span><span style=\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\"><span class=\"nowrap\">9</span></span></span></th><td scope=\"col\" rowspan=\"1\" style=\"padding:0 8px\"><i><a href=\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Battle_Frontier\" title=\"Pokémon: Battle Frontier\">Battle Frontier</a><a href=\"https://en.wikipedia.org/wiki/Pok%C3%A9mon:_Battle_Frontier\" title=\"Pokémon: Battle Frontier\">Battle Frontier</a>fwfwefwef</i></td><td colspan=\"2\">47</td><td colspan=\"1\" style=\"padding:0.2em 0.4em\">October 6, 2005</td><td style=\"padding:0 8px\">September 14, 2006</td></tr><tr style=\"height:100%\"><th scope=\"row\" colspan=\"1\" style=\"height:inherit;padding:0\"><span style=\"text-align:center;float:left;width:100%;height:100%\"><span style=\"width:14px;background:#B9F2FF;height:100%;float:left;box-shadow:inset -1px 0 #A2A9B1\"></span><span style=\"height:100%;width:calc(100% - 14px - 8px);display:flex;vertical-align:middle;align-items:center;justify-content:center;padding:0 4px\"><span class=\"nowrap\">10</span></span></span></th><td scope=\"col\" rowspan=\"1\" style=\"padding:0 8px\"><i><a href=\"https://en.wikipedia.org/wiki/Pok%C3%A9mon_the_Series:_Diamond_and_Pearl\" title=\"Pokémon the Series: Diamond and Pearl\">Diamond and Pearl</a></i></td><td colspan=\"2\">52</td><td colspan=\"1\" style=\"padding:0.2em 0.4em\">September 28, 2006</td><td style=\"padding:0 8px\">October 25, 2007</td></tr></tbody></table>")

(defn css-to-map [css-str]
  (->> (string/split css-str #";")
       (map string/trim)
       (map #(map string/trim (string/split % #":")))
       (map (fn [[k v]] [(keyword k) v]))
       (into {})))

(defn inner-text [hiccup-form]
  (-> (map
       #(cond
          (vector? %) (inner-text %)
          (string? %) %
          :else "")
       hiccup-form)
      string/join
      ;; Dealing with some weird unicode whitespace 
      ;; that some of the excel sheets had. 
      ;; They break rendering and in some cases the parser.
      (string/replace
       #"\u000C|\u000D|\u0020|\u0085|\u00A0|\u1680|\u2000|\u2001|\u2002|\u2003|\u2004|\u2005|\u2006|\u2007|\u2008|\u2009|\u200A|\u2028|\u2029|\u202F|\u205F|3000|\n"
       " ")
      (string/replace "   " " ")
      ;; Fixing special characters escaped by hickory so they're displayed normally.
      ;; There should be a better way to not url-encode strings in the first place.
      (string/replace "&amp;" "&")
      (string/replace "&nbsp;" " ")
      (string/replace "&lt;" "<")
      (string/replace "&gt;" ">")
      (string/replace "&quot;" "\"")))

(defn hiccup-cell->cell [hiccup-cell merge-until]
  (let [style (css-to-map (:style (second hiccup-cell)))]
    {:content (or
               (:data-bean-content (second hiccup-cell))
               (inner-text hiccup-cell) "")
     :style (merge
             (when (or (= (first hiccup-cell) :th)
                       (= (:font-weight style) "bold"))
               {:bold true})
             (when-let [bg (:background style)] {:background bg}))
   ;;  this is a weird place to sneak "merge"-ing cells but 
   ;;  passing it in from here for now to the grid so I can do everything in the 
   ;;  in the same function.
     :merge-until merge-until}))

(defn merged-with-another? [cell]
  (and (get-in cell [:style :merged-with])
       (not (get-in cell [:style :merged-until]))))

(defn cell->hiccup-cell [cell [r c]]
  (let [[mr mc] (get-in cell [:style :merged-until])]
    (when-not (merged-with-another? cell)
      [:td
       (merge
        {:data-bean-content (:content cell)
         :style (string/join ";"
                             [(when (get-in cell [:style :bold])
                                "font-weight: bold")
                              (when-let [bg (get-in cell [:style :background])]
                                (str "background: " (util/color-int->hex bg)))])}
        (when mc {:colspan (str (inc (- mc c)))})
        (when mr {:rowspan (str (inc (- mr r)))}))
       (:representation cell)])))

(defn hiccup-matrix->html [matrix]
  (hr/hiccup-to-html
   [[:table {}
     (into [:tbody {}] matrix)]]))

(defn hickory-table->cells [hickory-table]
  (loop [hiccup-cells (->>
                       (hs/select (hs/child (hs/tag :tr)) hickory-table)
                       (mapv #(hs/select (hs/child (hs/or (hs/tag :td) (hs/tag :th))) %))
                       (util/map-on-matrix-addressed (fn [idx cell] [idx (hc/hickory-to-hiccup cell)]))
                       (mapcat identity)
                       (sort-by first))
         occupieds #{}
         cells {}]
    (let [[idx hiccup-cell] (first hiccup-cells)
          [_ {:keys [colspan rowspan]}] hiccup-cell
          rowspan (and rowspan (js/parseInt rowspan))
          colspan (and colspan (js/parseInt colspan))
          [r c] (loop [idx* idx]
                  (if (get occupieds idx*)
                    (recur [(first idx*) (inc (second idx*))])
                    idx*))
          merge-until [(+ r (if (pos-int? rowspan) (dec rowspan) 0))
                       (+ c (if (pos-int? colspan) (dec colspan) 0))]
          got-occupied (mapcat identity (util/addresses-matrix [r c] merge-until))]
      (if (empty? hiccup-cells)
        cells
        (recur
         (rest hiccup-cells)
         (into occupieds got-occupied)
         (assoc cells [r c] (hiccup-cell->cell hiccup-cell (when (not= merge-until [r c]) merge-until))))))))

(defn plain-text->cells [text]
  (->> (string/split text "\n")
       (map #(string/split % "\t"))
       (map (partial map #(do {:content %})))
       (util/map-on-matrix-addressed (fn [idx cell] [idx cell]))
       (mapcat identity)
       (into {})))

(defn text->hickory-table [pasted-text]
  (->> pasted-text
       hickory/parse-fragment
       (map hickory/as-hickory)
       (map #(hs/select (hs/tag "table") %))
       (some not-empty)
       first))

(defn parse-table [e]
  (when-let [table (text->hickory-table
                    (.getData (.-clipboardData e) "text/html"))]
    (hickory-table->cells table)))

(defn parse-plaintext [e]
  (plain-text->cells
   (.getData (.-clipboardData e) "text")))

(defn selection->html
  [{:keys [start end]} sheet]
  (->> (util/addresses-matrix start end)
       (util/map-on-matrix #(cell->hiccup-cell (get-in (:grid sheet) %) %))
       (map #(into [] (remove nil? (into [:tr {}] %))))
       hiccup-matrix->html))

(defn selection->plain-text
  [{:keys [start end]} sheet]
  (->> (util/addresses-matrix start end)
       (util/map-on-matrix #(get-in (:grid sheet) (conj % :content)))
       (map #(string/join "\t" %))
       (string/join "\n")))
