(ns bean.log
  (:require [clojure.string :as str]))

(defn log [s & more]
  (print (str s (str/join " " more))))