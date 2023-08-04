(ns bean.core
  (:require [instaparse.core :as insta]))

(def ^:private parser (insta/parser
              "
              CellContents = UserExpression / Constant
              Integer = #'[0-9]+'
              String = #'.+'
              Constant = Integer / String

              CellAddress = #'[A-Z]+[1-9][0-9]*'
              UserExpression = '=' Expression
              Operation = '+'
              Expression = Value | CellAddress | Expression Operation Expression | FunctionInvocation
              FunctionInvocation = FunctionName '(' FunctionArguments ')'
              FunctionName = \"concat\"
              FunctionArguments = Epsilon | Expression | Expression ',' FunctionArguments

              Value = Integer / QuotedString
              QuotedString = '\"' QuotedRawString '\"'
              QuotedRawString = #'[^\"]+'
              "))

(defn parse [v]
  (insta/parse parser v))
