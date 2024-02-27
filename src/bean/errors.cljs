(ns bean.errors)

(defn reset [value]
  (dissoc value :error))

(defn mark [value error]
  (merge value error))

(defn get-error [{:keys [error]}]
  error)

(defn undefined-named-ref [name]
  {:error (str "Undefined reference: \"" name "\"")
   :representation (str "Undefined reference: \"" name "\"")})

(defn undefined-table-at [address]
  {:error (str "No table found at " address "")
   :representation (str "No table found at " address "")})

(defn invalid-table-args [address]
  {:error (str "table() needs a cell ref, given \"" address "\"")
   :representation (str "table() needs a cell ref, given \"" address "\"")})

(defn label-not-found [label-name]
  {:error (str "label \"" label-name "\" doesn't exist")
   :representation (str "label \"" label-name "\" doesn't exist")})

(defn spill-error []
  {:error "Spill error"
   :representation "Spill error"})

(defn stringified-error
  "This exists to funnel all usages of automatically stringified errors
   centrally. We should eventually remove this and have more explicit
   representations for each error."
  [e]
  {:error e
   :representation (str e)})

(defn matrix-size-mismatch-error []
  {:error "Matrices should be same size."
   :representation "Matrices should be same size."})

(defn type-mismatch-+-op []
  {:error "+ only works for Integers"
   :representation "+ only works for Integers"})

(defn type-mismatch-<-op []
  {:error "< only works for Integers"
   :representation "< only works for Integers"})

(defn type-mismatch->-op []
  {:error "> only works for Integers"
   :representation "> only works for Integers"})

(defn type-mismatch-=-op []
  {:error "= only works for Integers"
   :representation "= only works for Integers"})

(defn type-mismatch-*-op []
  {:error "* only works for Integers"
   :representation "* only works for Integers"})
