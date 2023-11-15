(ns bean.parser
  (:require [instaparse.core :as insta]))

(def ^:private statement-grammer
  "
    Program = Statement? { <'\n'+> Statement }
    <Statement> = LetStatement
    LetStatement = Name <{' '}> <':'> <{' '}> Expression
   ")

(def ^:private expression-grammer
  ;; TODO: Integers are currently just natural numbers
  "
    CellContents = <'='> Expression / RawValue / Epsilon
    <RawValue> =  Integer / String
    Integer = #'[0-9]+'
    String = #'.+'

    CellRef = #'[A-Z]+' #'[1-9][0-9]*'
    MatrixRef = CellRef <':'> CellRef

    Operation = '+' | '*'
    Expression = Value | CellRef | MatrixRef | Expression Operation Expression | FunctionInvocation | FunctionDefinition | Name
    FunctionInvocation = (FunctionDefinition | Name) <'('> [Expression {<' '> Expression}] <')'>
    FunctionDefinition = <'{'> Expression <'}'>
    Name = #'[a-z]+'

    Value = Integer / <'\"'> QuotedString <'\"'>
    QuotedString = #'[^\"]+'
    ")

(def ^:private parser
  (insta/parser expression-grammer))

(def ^:private statement-parser
  (insta/parser (str statement-grammer "\n" expression-grammer)))

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
