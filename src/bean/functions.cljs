(ns bean.functions)

(defn bean-concat [params]
  (let [concated-str (reduce (fn [c {:keys [value]}] (str c value))
                             ""
                             params)]
    {:value concated-str
     :representation concated-str}))