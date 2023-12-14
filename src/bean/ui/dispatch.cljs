(ns bean.ui.dispatch
  (:require [re-frame.core :as rf]
            [bean.ui.events :as events]))

(defn resize-col [col width]
  (rf/dispatch [::events/resize-col col width]))

(defn resize-row [row height]
  (rf/dispatch [::events/resize-row row height]))

(defn set-mode [rc mode]
  (rf/dispatch [::events/set-mode rc mode]))

(defn edit-mode [rc]
  (rf/dispatch [::events/set-mode rc :edit]))

(defn update-cell [rc content]
  (rf/dispatch [::events/update-cell rc content]))

(defn update-code [code]
  (rf/dispatch [::events/update-code code]))

(defn evaluate-code []
  (rf/dispatch [::events/evaluate-code]))

(defn display-help [flag]
  (rf/dispatch [::events/display-help flag]))