(ns bean.code-errors)

(defn get-error [sheet]
  (:code-error sheet))

(defn named-ref-error [named error]
  (str "name: " named ". " error))
