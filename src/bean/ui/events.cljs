(ns bean.ui.events
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.db :as db]
            [re-frame.core :as rf]
            [bean.code :as code]
            [bean.code-errors :as code-errors]))

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   (db/initial-app-db)))

;; TODO: think most of these should be reg-event-db instead of reg-event-fx
(rf/reg-event-fx
 ::update-code
 (fn update-code [{:keys [db]} [_ code]]
   {:db (update-in db [:sheet] #(-> %
                                    (code/set-code code)
                                    (assoc-in [:ui :code-evaluation-state] :pending)))}))

(rf/reg-event-fx
 ::evaluate-code
 (fn evaluate-code [{:keys [db]} _]
   {:db (-> db
            (update-in [:sheet] code/reevaluate)
            (assoc-in [:sheet :ui :code-evaluation-state]
                      (if (code-errors/get-error (:sheet db))
                        :error
                        :evaluated)))}))

(rf/reg-event-fx
 ::update-cell
 (fn update-cell [{:keys [db]} [_ address content]]
   {:db (update-in db [:sheet] #(grid/eval-cell address % content))}))

(rf/reg-event-fx
 ::resize-row
 (fn resize-row [{:keys [db]} [_ row height]]
   {:db (assoc-in db [:sheet :ui :row-heights row] height)}))

(rf/reg-event-fx
 ::resize-col
 (fn resize-col [{:keys [db]} [_ col width]]
   {:db (assoc-in db [:sheet :ui :col-widths col] width)}))

(rf/reg-event-fx
 ::set-mode
 (fn set-mode [{:keys [db]} [_ [r c] mode]]
   {:db (cond-> (assoc-in db [:sheet :grid r c :mode] mode)
          (= mode :edit) (assoc-in [:sheet :ui :selected-cell] [r c]))}))

(rf/reg-event-fx
 ::explain
 (fn explain [{:keys [db]} [_ expression]]
   {:db (assoc-in db
                  [:ui :provenance]
                  (provenance/sentence-proof expression (:sheet db)))}))

(rf/reg-event-fx
 ::display-help
 (fn display-help [{:keys [db]} [_ flag]]
   {:db (assoc-in db [:ui :help-display] flag)}))