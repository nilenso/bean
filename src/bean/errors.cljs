(ns bean.errors)

(defn undefined-named-ref [name]
  {:error (str "Undefined reference: \"" name "\"")
   :representation (str "Undefined reference: \"" name "\"")})

(defn named-ref-error [named error]
  (str "name: " named ". " error))