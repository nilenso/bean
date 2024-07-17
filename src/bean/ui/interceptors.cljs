(ns bean.ui.interceptors
  (:require [bean.ui.save :as save]
            [re-frame.core :as re-frame]))

(defn savepoint []
  (re-frame/->interceptor
   :id :savepoint
   :after (fn [context]
            (let [{:keys [db]} (:effects context)]
              (save/write-sheet (:sheet db)))
            context)))
