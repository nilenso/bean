(ns bean.ui.save
  (:require [cljs.reader :as reader]))

(def db-name "bean-db")
(def store-name "bean-store")
(def store-key "bean-sheet")
(def db-version 1)

(defn open-db []
  (js/Promise.
   (fn [resolve _reject]
     (let [request (.open js/indexedDB db-name db-version)]
       (set! (.-onupgradeneeded request)
             (fn [e]
               (let [db (.. e -target -result)]
                 (.createObjectStore db store-name))))
       (set! (.-onsuccess request)
             #(resolve (.. % -target -result)))))))

(defn write-sheet [sheet]
  (-> (open-db)
      (.then (fn [db]
               (js/Promise.
                (fn [resolve _reject]
                  (let [tx (.transaction db #js [store-name] "readwrite")
                        store (.objectStore tx store-name)
                        request (.put store
                                      (str (select-keys sheet [:grid
                                                               :depgraph
                                                               :frames
                                                               :last-frame-number
                                                               :grid-dimensions
                                                               :code-in-editor
                                                               :anthropic-api-key]))
                                      store-key)]
                    (set! (.-onsuccess request) resolve))))))))

(defn read-sheet []
  (-> (open-db)
      (.then (fn [db]
               (js/Promise.
                (fn [resolve _reject]
                  (let [tx (.transaction db #js [store-name] "readonly")
                        store (.objectStore tx store-name)
                        request (.get store store-key)]
                    (set! (.-onsuccess request)
                          #(let [result (.. % -target -result)]
                             (if result
                               (resolve (reader/read-string result))
                               (resolve nil)))))))))))
