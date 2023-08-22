(ns bean.core
  (:require [clojure.set :refer [union]]
            [cljs.math :refer [pow]]
            [instaparse.core :as insta]))

(def ^:private parser
  (insta/parser
  ;; TODO: Integers are currently just natural numbers
   "
    CellContents = <'='> Expression / Integer / String / Epsilon
    Integer = #'[0-9]+'
    String = #'.+'

    CellRef = #'[A-Z]+' #'[1-9][0-9]*'

    Operation = '+'
    Expression = Value | CellRef | Expression Operation Expression | FunctionInvocation
    FunctionInvocation = FunctionName <'('> FunctionArguments <')'>
    FunctionName = \"concat\"
    FunctionArguments = Epsilon | Expression | Expression <','> FunctionArguments

    Value = Integer / <'\"'> QuotedString <'\"'>
    QuotedString = #'[^\"]+'
    "))

(defn bean-op-+ [left right]
  (if (and (int? left) (int? right))
    (+ left right)
    {:error "Addition only works for Integers"}))

(defn parse [v]
  (insta/parse parser v))

(defn- has-subexpression? [[node-type & _]]
  (= node-type :Expression))

(defn- error-or-value [result]
  (if (:error result)
    result
    {:value result}))

(defn- formulas-map [cells f]
  (merge
   (error-or-value (apply f (map :value cells)))
   {:affected-cells (->> cells
                         (map :affected-cells)
                         (reduce union)
                         (into (set [])))}))

(defn- get-cell [grid address]
  (if-let [contents (get-in grid address)]
    contents
    {:error (str "Invalid address " address)}))

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
  ; ast goes down, value or an error comes up
  (let [[node-type arg] ast
        with-value #(do {:value %
                         :affected-cells affected-cells})
        eval-sub-ast #(eval-formula* grid {:ast %
                                           :affected-cells affected-cells})]
    (case node-type
      :CellContents (if arg
                      (let [{:keys [value affected-cells error]} (eval-sub-ast arg)]
                        (merge
                         {:content (str value)
                          :value value
                          :affected-cells affected-cells}
                         (when error {:error error})))
                      {:content ""
                       :value nil
                       :affected-cells affected-cells})
      :Integer (with-value (js/parseInt arg))
      :String (with-value arg)
      :CellRef (let [[_ a n] ast
                     cellref-ast (get-cell grid (a1->rc a (js/parseInt n)))
                     error (:error cellref-ast)]
                 (if error
                   {:value nil
                    :error error
                    :affected-cells affected-cells}
                   (eval-sub-ast cellref-ast)))
      :Operation (with-value (case arg
                               "+" bean-op-+))
      :Expression (if-not (has-subexpression? arg)
                    (eval-sub-ast arg)
                    (let [[_ left op right] ast]
                      (formulas-map [(eval-sub-ast op)
                                     (eval-sub-ast left)
                                     (eval-sub-ast right)]
                                    #(apply %1 [%2 %3]))))
      :Value (eval-sub-ast arg))))

(defn- eval-formula [grid address ast]
  (eval-formula* grid
                 {:affected-cells (set [address])
                  :ast ast}))

;; TODO: Is there a better way to return vectors instead of lists
;; for O(1) lookups later.
(defn map-on-matrix [f matrix]
  (vec (map #(vec (map (fn [contents] (f contents)) %)) matrix)))

(defn map-on-matrix-addressed [f matrix]
  (vec (map-indexed (fn [row-idx row]
                      (vec (map-indexed
                            (fn [col-idx contents]
                              (f [row-idx col-idx] contents))
                            row)))
                    matrix)))

(defn evaluate-grid [grid]
  (let [parsed-grid (map-on-matrix parse grid)]
    (->> parsed-grid
        (map-on-matrix-addressed #(eval-formula parsed-grid %1 %2)))))
