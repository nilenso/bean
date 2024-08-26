(ns bean.ui.features
  (:require [clojure.string :as string]))

(def show-control-bar true)
(defn llm-labelling? [sheet]
  (not (clojure.string/blank? (get-in sheet [:anthropic-api-key]))))
