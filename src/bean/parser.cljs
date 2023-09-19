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
    Expression = Value | CellRef | MatrixRef | Expression Operation Expression | FunctionInvocation
    FunctionInvocation = Name <'('> [Expression {<' '> Expression}] <')'>
    Name = #'[a-z]+'
    FunctionArguments = Epsilon | Expression | Expression <','> FunctionArguments

    Value = Integer / <'\"'> QuotedString <'\"'>
    QuotedString = #'[^\"]+'
    "))

(defn parse [v]
  (insta/parse parser v))
