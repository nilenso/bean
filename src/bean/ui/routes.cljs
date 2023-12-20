(ns bean.ui.routes
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [re-frame.core :as rf]
            [bean.ui.events :as events]))

(def ^:private app-routes
  ["/" {"" :root}])

(defn- set-page! [match]
  (rf/dispatch-sync [::events/set-route match]))

(def ^:private history
  (pushy/pushy set-page! (partial bidi/match-route app-routes)))

(defn start []
  (pushy/start! history))