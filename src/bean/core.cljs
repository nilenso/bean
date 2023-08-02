(ns bean.core
  (:require [instaparse.core :as insta]))

(def ^:private parser (insta/parser
              "
              CellContents = UserExpression / Constant
              Integer = #'[0-9]+'
              String = #'.+'
              Constant = Integer / String

              UserExpression = '=' Expression
              Expression = Value
              Value = Integer / QuotedString
              QuotedString = '\"' QuotedRawString '\"'
              QuotedRawString = #'[^\"]+'
              "))

(defn parse [v]
  (insta/parse parser v))

(comment :grammer
  "Constant = Integer | LiterallyAnythingString"
  "CellContents = Constant | '=' Expression"

  "CellAddress = CapitalLetter+ Number+"
  "Operation = '+'"
  "Integer = Number+"
  "String = '"' Anything '"'"
  "Value = Integer | String"
  "Expression = CellAddress | Value  | Expression Operation Expression | FunctionInvocation"
  "FunctionInvocation = FunctionName '(' FunctionArguments ')'"
  "FunctionArguments = NULL | Expression | Expression ',' FunctionArguments"
  )

(comment :samples
         "4"
         "=4 + 9"
         "foo"
         "=C8"
         "=concat('foo', C98)")


