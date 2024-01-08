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

(re-frame/reg-sub
 ::editing-cell
 (fn [db]
   (get-in db [:ui :grid :editing-cell])))

(re-frame/reg-sub
 ::pixi-app
 (fn [db]
   (get-in db [:ui :pixi-app])))

(re-frame/reg-sub
 ::route
 (fn [db]
   (:route db)))
