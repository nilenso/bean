(ns bean.code-errors)

(defn named-ref-error [named error]
  (str "name: " named ". " error))
