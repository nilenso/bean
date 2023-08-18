(ns bean.core
  (:require [clojure.set :refer [union]]
            [cljs.math :refer [pow]]
            [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
    "
    CellContents = UserExpression / Constant
    Integer = #'[0-9]+'
    String = #'.+'
    Constant = Integer / String

    CellRef = #'[A-Z]+' #'[1-9][0-9]*'

    UserExpression = <'='> Expression
    Operation = '+'
    Expression = Value | CellRef | Expression Operation Expression | FunctionInvocation
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

(def ^:private get-cell get)

(def ^:private num-alphabets 26)

(defn- a1->rc [a n]
  (let [indexed-a (map vector (reverse a) (range))
        c (reduce (fn [total [alphabet i]]
                    (+ total
                       (* (pow num-alphabets i)
                          (- (.charCodeAt alphabet 0)
                             (dec (.charCodeAt "A" 0))))))
                  0
                  indexed-a)]
    [(dec n) (dec c)]))

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
      :CellRef (let [[_ a n] ast]
                  (eval-sub-ast (get-cell grid (a1->rc a (js/parseInt n)))))
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
