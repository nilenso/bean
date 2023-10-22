(ns bean.scratch 
  (:require [bean.interpreter :as interpreter]
            [bean.parser :as parser]))

(defn get-code [{:keys [code]}]
  (or 
   code
   "double: {x+x}"))

(defn set-code [sheet code]
  (-> sheet
    (assoc-in [:code] code)
    (assoc-in [:code-ast] (parser/parse-statement code))))


(defn reevaluate [sheet]
  (interpreter/reset-and-eval-code sheet))
