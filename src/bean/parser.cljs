(ns bean.parser
  (:require [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
  ;; TODO: Integers are currently just natural numbers
   "
    CellContents = <'='> Expression / Integer / String / Epsilon
    Integer = #'[0-9]+'
    String = #'.+'

    CellRef = #'[A-Z]+' #'[1-9][0-9]*'
    MatrixRef = CellRef <':'> CellRef

    Operation = '+'
    Expression = Value | CellRef | MatrixRef | Expression Operation Expression | FunctionInvocation | FunctionDefinition | Name
    FunctionInvocation = (FunctionDefinition | Name) <'('> [Expression {<' '> Expression}] <')'>
    FunctionDefinition = <'{'> Expression <'}'>
    Name = #'[a-z]+'

    Value = Integer / <'\"'> QuotedString <'\"'>
    QuotedString = #'[^\"]+'
    "))

(defn parse [v]
  (insta/parse parser v))
