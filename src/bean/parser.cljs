(ns bean.parser
  (:require [instaparse.core :as insta]))

(def ^:private cell-parser
   (insta/parser "
     CellContents = <'='> Expression / String / Epsilon
      
      String = #'.+'
      CellRef = #'[A-Z]+' #'[1-9][0-9]*'
      MatrixRef = CellRef <':'> CellRef
      
      Expression = Value | CellRef | MatrixRef | Name | FunctionDefinition | Expression Operation Expression
      Operation = '+'
      
      Value = Integer / <'\"'> QuotedString <'\"'>
      Integer = #'[0-9]+'
      QuotedString = #'[^\"]+'
      
      FunctionDefinition = <'{'> Expression <'}'>
      Name = #'[a-z0-9]+(?:-[a-z0-9]+)*'
     ")
  )

(def ^:private program-parser
  (insta/parser
  ;; TODO: Integers are currently just natural numbers 
   (str 
    "
    Program = {Statement}
    Statement = LetExpression
    LetExpression = <'let'> <' '> Name <' '> Expression <'\n'>
    "
    cell-parser)))

(defn parse-cell [v]
  (insta/parse cell-parser v))

(defn parse-program [v]
  (insta/parse program-parser v))
