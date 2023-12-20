(ns bean.ui.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::sheet
 (fn [db]
   (:sheet db)))

(re-frame/reg-sub
 ::ui
 (fn [db]
   (:ui db)))
