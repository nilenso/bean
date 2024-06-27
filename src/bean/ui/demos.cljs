(ns bean.ui.demos
  (:require [cljs.reader :as reader]))

(def file-name "demos.edn")
(def db-name "bean-demos")
(def object-store "bean-objects")
(def demos-key "demos")

(defn fetch-and-parse-edn [url]
  (-> (js/fetch url)
      (.then #(.text %))))

(defn get-demos []
  (js/Promise.
   (fn [resolve _]
     (let [request (.open js/indexedDB db-name 1)]
       (set! (.-onsuccess request)
             (fn [e]
               (let [db (.. e -target -result)
                     txn (.transaction db #js [object-store] "readonly")
                     store (.objectStore txn object-store)
                     get-request (.get store demos-key)]
                 (set! (.-onsuccess get-request)
                       #(resolve (reader/read-string (.. % -target -result)))))))))))

(defn get-demo [demo-name]
  (.then (get-demos)
         #(get % demo-name)))

(defn save-demos-locally [file]
  (let [request (.open js/indexedDB db-name 1)]
    (set! (.-onupgradeneeded request)
          (fn [e]
            (let [db (.. e -target -result)]
              (when (not (.contains (.-objectStoreNames db) object-store))
                (.createObjectStore db object-store)))))

    (set! (.-onsuccess request)
          (fn [e]
            (let [db (.. e -target -result)
                  txn (.transaction db #js [object-store] "readwrite")
                  store (.objectStore txn object-store)]
              (.put store file demos-key))))))

(defn fetch-demos []
  (.then (fetch-and-parse-edn (str "/" file-name))
         #(save-demos-locally %)))

(defn download-edn-as-file
  "Takes data, a filename, and an optional MIME type, and triggers a file download."
  [data]
  (let [blob-data (str data)
        blob (js/Blob. #js [blob-data] #js {:type "application/edn"})
        url (.createObjectURL js/URL blob)
        link (.createElement js/document "a")]
    (set! (.-href link) url)
    (set! (.-download link) file-name)
    (.setAttribute link "download" file-name)
    (.appendChild (.-body js/document) link)
    (.click link)
    (.removeChild (.-body js/document) link)
    (.revokeObjectURL js/URL url)))
