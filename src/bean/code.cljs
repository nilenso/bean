(ns bean.code
  (:require [bean.grid :as grid]))

(defn reevaluate [{:keys [code-in-editor] :as sheet}]
  (grid/eval-code sheet code-in-editor))

(defn set-code [sheet code]
  (assoc sheet :code-in-editor code))

(defn get-code [sheet]
  (get sheet :code-in-editor))
