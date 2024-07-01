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

(defn undefined-frame-at [address]
  {:error (str "No frame found at " address "")
   :representation (str "No frame found at " address "")})

(defn invalid-frame-args [address]
  {:error (str "frame() needs a cell ref, given \"" address "\"")
   :representation (str "frame() needs a cell ref, given \"" address "\"")})

(defn label-not-found [label-name]
  {:error (str "label \"" label-name "\" doesn't exist")
   :representation (str "label \"" label-name "\" doesn't exist")})

(defn function-not-found []
  {:error "function not found"
   :representation "function not found"})

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

(defn divide-by-zero []
  {:error "cannot divide by zero"
   :representation "cannot divide by zero"})

(defn type-mismatch-op [op-str]
  (let [e (str op-str " only works for Integers")]
    {:error e :representation e}))
