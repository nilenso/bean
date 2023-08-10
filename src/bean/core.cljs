(ns bean.core
  (:require [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
    "
    CellContents = UserExpression / Constant
    Integer = #'[0-9]+'
    String = #'.+'
    Constant = Integer / String

    Ref = #'[A-Z]+[1-9][0-9]*'
    UserExpression = '=' Expression
    Operation = '+'
    Expression = Value | Ref | Expression Operation Expression | FunctionInvocation
    FunctionInvocation = FunctionName '(' FunctionArguments ')'
    FunctionName = \"concat\"
    FunctionArguments = Epsilon | Expression | Expression ',' FunctionArguments

    Value = Integer / QuotedString
    QuotedString = '\"' QuotedRawString '\"'
    QuotedRawString = #'[^\"]+'
    "))

(defn parse [v]
  (insta/parse parser v))


(def grid
  {"A1" "1"
   "A2" "2"
   "A3" "=A1+A2"
   "A4" "=A3+1"})

(def evaluated-grid
  {"A1" "1"
   "A2" "2"
   "A3" "3"})

(let [[x y z :as xyz] [1 23 848 84 47 9]] [x y z xyz])

(defn- has-subexpression? [[node-type & _]]
  (= node-type :Expression))

(defn eval-formula [grid formula]
  (let [[node-type arg1 arg2 :as ast-node] formula
        eval-sub #(eval-formula grid %)]
    (case node-type
      :CellContents (eval-sub arg1)
      :Integer (js/parseInt arg1)
      :String arg1
      :Constant (eval-sub arg1)
      :Ref (eval-sub (get grid arg1))
      :UserExpression (str (eval-sub arg2))
      :Operation (case arg1
                   "+" #(+ %1 %2))
      :Expression (if (has-subexpression? arg1)
                    (let [[_ left op right] ast-node]
                      (apply
                        (eval-sub op)
                        [(eval-sub left)
                         (eval-sub right)]))
                    (eval-sub arg1))
      :Value (eval-sub arg1))))

(defn evaluate-grid [grid]
  (let [parsed-grid (update-vals grid parse)]
    (update-vals parsed-grid #(eval-formula parsed-grid %))))
