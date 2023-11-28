(ns bean.functions)

(defn bean-concat [params]
  (let [concated-str (reduce (fn [c {:keys [scalar]}] (str c scalar))
                             ""
                             params)]
    {:scalar concated-str
     :representation concated-str}))

(defn bean-error [params]
  (let [str-err (str (:error (first params)))]
    {:scalar str-err
     :representation str-err}))