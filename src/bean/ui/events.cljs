(ns bean.ui.events
  (:require [bean.grid :as grid]
            [bean.tables :as tables]
            [bean.ui.provenance :as provenance]
            [bean.ui.db :as db]
            [re-frame.core :as rf]
            [reagent.core :as rc]
            [bean.code :as code]
            [bean.code-errors :as code-errors]
            [bean.ui.util :as util]))

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
   (update-in db [:sheet] #(grid/update-cell address % content))))

(rf/reg-event-fx
 ::merge-cells
 (fn merge-cells [{:keys [db]} [_ area]]
   {:db (update-in db [:sheet] #(grid/merge-cells % area))
    :fx [[:dispatch [::edit-cell (:start area)]]]}))

(rf/reg-event-fx
 ::unmerge-cells
 (fn unmerge-cells [{:keys [db]} [_ addresses]]
   {:db (update-in db [:sheet] #(grid/unmerge-cells % addresses))
    :fx [[:dispatch [::edit-cell (first addresses)]]]}))

(rf/reg-event-db
 ::set-cell-backgrounds
 (fn set-cell-backgrounds [db [_ addresses background]]
   (update-in db [:sheet] #(grid/set-cell-backgrounds % addresses background))))

(rf/reg-event-db
 ::toggle-cell-bold
 (fn toggle-cell-bold [db [_ addresses]]
   (update-in db [:sheet] #(grid/toggle-cell-bolds % addresses))))

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
   (let [rc* (util/merged-or-self rc (:sheet db))]
     {:db (assoc-in db [:ui :grid :editing-cell] rc*)
      :fx [[:dispatch [::set-selection {:start rc* :end (util/merged-until-or-self rc* (:sheet db))}]]
           [::focus-element "cell-input"]]})))

(rf/reg-event-db
 ::clear-edit-cell
 (fn clear-edit-cell [db [_]]
   (assoc-in db [:ui :grid :editing-cell] nil)))

(rf/reg-event-db
 ::set-selection
 (fn [db [_ selection]]
   (assoc-in db [:ui :grid :selection] selection)))

(rf/reg-event-db
 ::making-table
 (fn making-table [db [_]]
   (assoc-in db [:ui :making-table] true)))

(rf/reg-event-fx
 ::select-table
 (fn select-table [{:keys [db]} [_ table-name]]
   (when-let [{:keys [start]} (get-in db [:sheet :tables table-name])]
     {:db (assoc-in db [:ui :grid :selected-table] table-name)
      :fx [[:dispatch [::edit-cell start]]]})))

(rf/reg-event-fx
 ::make-table
 (fn make-table [{:keys [db]} [_ table-name area]]
   {:db (->  db
             (assoc-in [:ui :making-table] false)
             (update-in [:sheet] #(tables/make-table % table-name area)))
    :fx [[:dispatch [::select-table table-name]]]}))

(rf/reg-event-db
 ::add-labels
 (fn add-labels [db [_ table-name addresses dirn]]
   (update-in db [:sheet]
              #(tables/add-labels % table-name addresses dirn))))

(rf/reg-event-db
 ::remove-labels
 (fn remove-labels [db [_ table-name addresses]]
   (->  db
        (update-in [:sheet] #(tables/unmark-skipped % table-name addresses))
        (update-in [:sheet] #(tables/remove-labels % table-name addresses)))))

(rf/reg-event-db
 ::mark-skip-cells
 (fn mark-skip-cells [db [_ table-name addresses]]
   (update-in db [:sheet]
              #(tables/mark-skipped % table-name addresses))))

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
