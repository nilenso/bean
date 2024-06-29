(ns bean.parser.trellis-parser
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [bean.parser.parser :as parser]
            [instaparse.core :as insta]))

;; This namespace has parsing functionality for Trellis, the cli program
;; It defines the parsing for the .leaf file format.

(def ^:private test-grammer
  "
    TestProgram = Epsilon | (TestStatement { <'\n'+> TestStatement })
    <TestStatement> = AssertionStatement
    AssertionStatement = Expression <{' '}> <'='> <{' '}> Expression
   ")

(def ^:private csv-grid-grammer
  "Note: This doesn't bother with escaping yet."

  "
   CommaSeparatedContent = CSCRow { <'\n'> CSCRow }
   CSCRow = CSContent <','> CSContent { <','> CSContent }
   <CSContent> = #'[^,\n]*'
   ")

(def ^:private trellis-grammer
  "Note: This doesn't bother with escaping yet. Fragile & breaks with any excess '%'"

  "
   TrellisFile = Program <SectionSep> CommaSeparatedContent <SectionSep> TestProgram ['\n']
   SectionSep = '\n'+ '%' '\n'+
   ")

(def ^:private trellis-parser
  (insta/parser (str/join "\n" [trellis-grammer
                                test-grammer
                                csv-grid-grammer
                                parser/statement-grammer
                                parser/expression-grammer])))

(defn- walk-remove-keywords [nested-structure]
  (walk/postwalk
   #(if (vector? %)
      (->> %
           (remove keyword?)
           (into []))
      %)
   nested-structure))

(defn parse [content]
  (update-in (->> content
                  (insta/parse trellis-parser)
                  (insta/add-line-and-column-info-to-metadata content))
             [2] ;; Remove parser keywords from the CSV content grid
             walk-remove-keywords))

(defn trellis-subs [src trellis-ast]
  (apply subs src (insta/span trellis-ast)))

(def ^:private tests-parser
  (insta/parser (str/join "\n" [test-grammer
                                parser/expression-grammer])))

(defn parse-tests [content]
  (insta/parse tests-parser content))
