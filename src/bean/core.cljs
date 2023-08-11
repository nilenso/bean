(ns bean.core
  (:require [clojure.set :refer [union]]
            [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
    "
    CellContents = UserExpression / Constant
    Integer = #'[0-9]+'
    String = #'.+'
    Constant = Integer / String

    Ref = #'[A-Z]+[1-9][0-9]*'
    UserExpression = <'='> Expression
    Operation = '+'
    Expression = Value | Ref | Expression Operation Expression | FunctionInvocation
    FunctionInvocation = FunctionName <'('> FunctionArguments <')'>
    FunctionName = \"concat\"
    FunctionArguments = Epsilon | Expression | Expression <','> FunctionArguments

    Value = Integer / QuotedString
    QuotedString = <'\"'> QuotedRawString <'\"'>
    QuotedRawString = #'[^\"]+'
    "))

(defn parse [v]
  (insta/parse parser v))

(defn- has-subexpression? [[node-type & _]]
  (= node-type :Expression))

(defn- formulas-map [cells f]
  (assoc (apply f (map :value cells))
         :affected-cells (->> cells 
                              (map :affected-cells)
                              (reduce union)
                              (into (set [])))))

(defn- eval-formula* [grid {:keys [ast affected-cells]}]
  ; ast goes down, value comes up
  (let [[node-type arg] ast
        with-value #(do {:value %
                         :affected-cells affected-cells})
        eval-sub-ast #(eval-formula* grid {:ast %
                                           :affected-cells affected-cells})]
    (case node-type
      :CellContents  (let [{:keys [value affected-cells]} (eval-sub-ast arg)]
                       {:content (str value)
                        :value value
                        :affected-cells affected-cells})
      :Integer (with-value (js/parseInt arg))
      :String (with-value arg)
      :Constant (eval-sub-ast arg)
      :Ref (eval-sub-ast (get grid arg))
      :UserExpression (eval-sub-ast arg)
      :Operation (with-value (case arg
                   "+" #(+ %1 %2)))
      :Expression (if-not (has-subexpression? arg)
                    (eval-sub-ast arg)
                    (let [[_ left op right] ast]
                      (formulas-map [(eval-sub-ast op)
                                     (eval-sub-ast left)
                                     (eval-sub-ast right)]
                                    #(do {:value (apply %1 [%2 %3])}))))
      :Value (eval-sub-ast arg))))

(defn- eval-formula [grid [address ast]]
  [address
  (eval-formula* grid
                 {:affected-cells (set [address])
                  :ast ast
                  :value nil})])

(defn evaluate-grid [grid]
  (let [parsed-grid (update-vals grid parse)]
    (->> parsed-grid
        (map #(eval-formula parsed-grid %))
        (into {}))))
