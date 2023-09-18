(ns bean.parser
  (:require [instaparse.core :as insta]))

(def ^:private cell-grammer "
     CellContents = <'='> Expression / Integer / String / Epsilon
      
      String = #'.+'
      CellRef = #'[A-Z]+' #'[1-9][0-9]*'
      MatrixRef = CellRef <':'> CellRef
      
      Expression = Value / FunctionInvocation / FunctionDefinition / CellRef / MatrixRef / Name / Expression Operation Expression
      Operation = '+'
      
      Value = Integer / <'\"'> QuotedString <'\"'>
      Integer = #'[0-9]+'
      QuotedString = #'[^\"]+'
      
      FunctionDefinition = <'{'> Expression <'}'>
      FunctionInvocation = (Name | FunctionDefinition) <'('> [Expression {<' '> Expression}] <')'>
      Name = #'[a-z0-9]+(?:-[a-z0-9]+)*'
                             ")
(def ^:private cell-parser
   (insta/parser cell-grammer))

(defn parse-cell [v]
  (insta/parse cell-parser v))

(comment This is all unnecessary for a while

         (def ^:private program-parser
           (insta/parser
  ;; TODO: Integers are currently just natural numbers 
            (str 
             "
    Program = {Statement}
    Statement = LetExpression
    LetExpression = <'let'> <' '> Name <' '> Expression <'\n'>
    "
             cell-grammer)))

(defn parse-program [v]
  (insta/parse program-parser v))
         )