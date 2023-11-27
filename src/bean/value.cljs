(ns bean.value
  (:require [bean.parser.parser :as parser]))

(defn from-statement
  [content ast]
  {:content content
   :ast  ast})

(defn from-cell [content]
  {:content content
   :ast (parser/parse content)})

