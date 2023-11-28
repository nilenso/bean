(ns bean.trellis
  (:require [bean.parser.trellis-parser :as trellis-parser]
            [bean.log :refer [log]]
            [bean.grid :as grid]
            [clojure.string :as str]))

(def ^:private all-tests-pass (atom true))

(defn execute [filepath]
  (if-let [contents (str/replace (str (.readFileSync (js/require "fs")
                                                     filepath))
                                 "\r\n" ;; Replace windows newlines with linux newlines
                                 "\n")]
    (let [[_ code grid test] (trellis-parser/parse (str contents))
          sheet (-> (grid/new-sheet grid
                                    (str
                                     (trellis-parser/trellis-subs contents code))
                                    (str
                                     (trellis-parser/trellis-subs contents test)))
                    grid/eval-sheet
                    (assoc :leaf-contents contents))]
      (doall
       (->> (grid/eval-test sheet)
            (remove first) ;; filter test failures only
            (map
             (fn [[_ lval rval]]
               (reset! all-tests-pass false)
               (log "Assertion failed. "
                    (:content lval)
                    "("
                    (:scalar lval)
                    ") != "
                    (:content rval)
                    "("
                    (:scalar rval)
                    ")"))))))
    (log "Error loading file")))

(defn main [filepath]
  (log "Evaluating " filepath)
  (execute filepath)
  (when-not @all-tests-pass
    (.exit (js/require "process") 1)))