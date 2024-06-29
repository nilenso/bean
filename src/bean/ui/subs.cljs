(ns bean.ui.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::sheet
 (fn [db]
   (:sheet db)))

(re-frame/reg-sub
 ::frames
 (fn [db]
   (get-in db [:sheet :frames])))

(re-frame/reg-sub
 ::ui
 (fn [db]
   (:ui db)))

(re-frame/reg-sub
 ::editing-cell
 (fn [db]
   (get-in db [:ui :grid :editing-cell])))

(re-frame/reg-sub
 ::selection
 (fn [db]
   (get-in db [:ui :grid :selection])))

(re-frame/reg-sub
 ::demo-names
 (fn [db]
   (get-in db [:ui :demo-names])))

(re-frame/reg-sub
 ::renaming-frame
 (fn [db]
   (get-in db [:ui :renaming-frame])))

(re-frame/reg-sub
 ::current-demo-name
 (fn [db]
   (get-in db [:ui :current-demo-name])))

(re-frame/reg-sub
 ::route
 (fn [db]
   (:route db)))
