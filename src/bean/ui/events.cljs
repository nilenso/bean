(ns bean.ui.events
  (:require [bean.grid :as grid]
            [bean.ui.provenance :as provenance]
            [bean.ui.db :as db]
            [re-frame.core :as rf]
            [reagent.core :as rc]
            [bean.code :as code]
            [bean.code-errors :as code-errors]))

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   (db/initial-app-db)))

(rf/reg-event-db
 ::update-code
 (fn update-code [db [_ code]]
   (update-in db [:sheet] #(-> %
                               (code/set-code code)
                               (assoc :code-evaluation-state :pending)))))

(rf/reg-event-db
 ::evaluate-code
 (fn evaluate-code [db _]
   (-> db
       (update-in [:sheet] code/reevaluate)
       (assoc-in [:sheet :code-evaluation-state]
                 (if (code-errors/get-error (:sheet db))
                   :error
                   :evaluated)))))

(rf/reg-event-db
 ::update-cell
 (fn update-cell [db [_ address content]]
   (update-in db [:sheet] #(grid/eval-cell address % content))))

(rf/reg-event-db
 ::merge-cells
 (fn merge-cells [db [_ start end]]
   (update-in db [:sheet] #(grid/merge-cells % start end))))

(rf/reg-event-db
 ::unmerge-cells
 (fn merge-cells [db [_ addresses]]
   (update-in db [:sheet] #(grid/unmerge-cells % addresses))))

(rf/reg-event-db
 ::set-cell-backgrounds
 (fn set-cell-backgrounds [db [_ addresses background]]
   (update-in db [:sheet] #(grid/set-cell-backgrounds % addresses background))))

(rf/reg-event-db
 ::toggle-cell-bold
 (fn toggle-cell-bold [db [_ addresses]]
   (update-in db [:sheet] #(grid/toggle-cell-bolds % addresses))))

(rf/reg-event-db
 ::make-table
 (fn make-table [db [_ table-name start end]]
   (update-in db [:sheet] #(grid/make-table % table-name start end))))

(rf/reg-event-fx
 ::submit-cell-input
 (fn submit-cell-input [{:keys [db]} [_ content]]
   {:fx [[:dispatch [::update-cell (get-in db [:ui :grid :editing-cell]) content]]]}))

(rf/reg-event-db
 ::resize-row
 (fn resize-row [db [_ row height]]
   (assoc-in db [:sheet :grid-dimensions :row-heights row] height)))

(rf/reg-event-db
 ::resize-col
 (fn resize-col [db [_ col width]]
   (assoc-in db [:sheet :grid-dimensions :col-widths col] width)))

(rf/reg-fx
 ::focus-element
 (fn [el-id]
   (rc/after-render  #(some-> js/document (.getElementById el-id) .focus))))

(rf/reg-event-fx
 ::edit-cell
 (fn edit-cell [{:keys [db]} [_ rc]]
   {:db (assoc-in db [:ui :grid :editing-cell] rc)
    :fx [[::focus-element "cell-input"]]}))

(rf/reg-event-db
 ::set-selection
 (fn [db [_ selection]]
   (assoc-in db [:ui :grid :selection] selection)))

(rf/reg-event-fx
 ::select-table
 (fn select-table [_ [_ {:keys [start end]}]]
   {:fx [[:dispatch [::edit-cell nil]]
         [:dispatch [::set-selection {:start start :end end :type :table}]]]}))

(rf/reg-event-db
 ::clear-selection
 (fn [db [_]]
   (assoc-in db [:ui :grid :selection] nil)))

(rf/reg-event-db
 ::explain
 (fn explain [db [_ expression]]
   (assoc-in db
             [:ui :provenance]
             (provenance/sentence-proof expression (:sheet db)))))

(rf/reg-event-db
 ::display-help
 (fn display-help [db [_ flag]]
   (assoc-in db [:ui :help-display] flag)))

(rf/reg-event-db
 ::set-route
 (fn set-route [db [_ match]]
   (assoc-in db [:route] match)))
