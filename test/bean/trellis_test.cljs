(ns bean.trellis-test
  (:require [cljs-node-io.fs :as fs]
            [bean.trellis :as trellis]
            [clojure.test :refer [deftest testing]]))

(deftest trellis-test
  (testing "Run all the trellis tests"
    (fs/crawl "./test/trellis" (fn [path]
                                 (when (and (fs/file? path)
                                            (re-matches #".*\.leaf$" path))
                                   (print "Testing" (str (-> path fs/dirname fs/basename) "/" (fs/basename path)))
                                   (trellis/execute path))))))