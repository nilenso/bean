(ns bean.parser.parser
  (:require [instaparse.core :as insta]))

(def statement-grammer
  "
    Program = Statement? { <'\n'+> Statement }
    <Statement> = LetStatement
    LetStatement = Name <{' '}> <':'> <{' '}> Expression
   ")

(def expression-grammer
  ;; TODO: Integers are currently just natural numbers
  "
    CellContents = <'='> Expression / RawValue / Epsilon
    <RawValue> =  Number / String
    Number = #'[-]?[0-9]+([.][0-9]+)?'
    String = #'.*'

    CellRef = #'[A-Z]+' #'[1-9][0-9]*'
    MatrixRef = CellRef <':'> CellRef

    Operation = '+' | '*' | '=' | '<' | '>' | '/' | '-'
    Expression = (Value | CellRef | MatrixRef | FunctionChain |
                Expression Operation Expression | FunctionInvocation |
                FunctionDefinition | FrameLookup) / Name

    FunctionInvocation = (FunctionDefinition | Name) <'('> [Expression {<','> Expression}] <')'>
    FunctionDefinition = <'{'> Expression <'}'>
    FunctionChain = Expression [<'.'> (FunctionInvocation | LabelLookup)]

    Name = #'[a-zA-Z0-9 ]+(?<! )'
    FrameLookup = <'$'> Name
    LabelLookup = Name

    Value = Number / <'\"'> QuotedString <'\"'>
    QuotedString = #'[^\"]+'
    ")

(def ^:private parser
  (insta/parser expression-grammer :auto-whitespace :standard))

(def ^:private statement-parser
  (insta/parser (str statement-grammer "\n" expression-grammer) :auto-whitespace :standard))

(defn parse-statement [src]
  (let [program (insta/parse statement-parser src)]
    (insta/add-line-and-column-info-to-metadata src program)))

(defn parse [v]
  (insta/parse parser v))

(defn statement-source [code statement]
  (apply subs code (insta/span statement)))

(defn error [result]
  (when (insta/get-failure result)
    (let [{:keys [index reason]} result]
      (str "Parse Error: idx " index ". " reason))))
