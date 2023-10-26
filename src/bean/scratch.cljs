(ns bean.scratch 
  (:require [bean.interpreter :as interpreter]
            [bean.parser :as parser]))

(defn get-code [{:keys [code] :as sheet}]
  (or 
   code
   "double: {x+x}"))

(defn set-code [sheet code]
  (prn "CODEEE" code)
  (prn "SATETMENT" (parser/parse-statement code))
  (->  sheet
    (assoc-in [:code] code)
    (assoc-in [:code-ast] (parser/parse-statement code))))


(defn reevaluate [{:keys [code-ast] :as sheet}]
  (interpreter/reset-and-eval-code sheet))
