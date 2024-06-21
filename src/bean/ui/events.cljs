(ns bean.ui.events
  (:require [bean.code :as code]
            [bean.code-errors :as code-errors]
            [day8.re-frame.undo :as undo :refer [undoable]]
            [bean.frames :as frames]
            [bean.grid :as grid]
            [bean.ui.db :as db]
            [bean.ui.paste :as paste]
            [bean.ui.provenance :as provenance]
            [bean.ui.util :as util]
            [re-frame.core :as rf]
            [reagent.core :as rc]))

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
 ::reload-bindings
 (fn reload-bindings [db [_]]
   (update-in db [:sheet :bindings] merge grid/default-bindings)))

(rf/reg-event-db
 ::update-cell
 (undoable)
 (fn update-cell [db [_ address content]]
   (update-in db [:sheet] #(grid/update-cell-content address % content))))

(rf/reg-event-db
 ::clear-area
 (undoable)
 (fn clear-area [db [_ area]]
   (update-in db [:sheet] #(grid/clear-area % area))))

(rf/reg-event-fx
 ::handle-global-kbd
 (fn handle-global-kbd [{:keys [db]} [_ e]]
   (let [selection (get-in db [:ui :grid :selection])]
     (cond
       (and (= (.-key e) "z") (or (.-ctrlKey e) (.-metaKey e))
            (.-shiftKey e))
       (when (undo/redos?) (rf/dispatch [:redo]))

       (and (= (.-key e) "z") (or (.-ctrlKey e) (.-metaKey e)))
       (when (undo/undos?) (rf/dispatch [:undo]))

       (or (.-ctrlKey e) (.-metaKey e)
           (= (.-key e) "Shift")
           (= (.-key e) "Escape")) nil

       :else
       (when-let [[r c] (:start selection)]
         (let [[mr mc] (get-in db [:sheet :grid r c :style :merged-until])]
           (if (or (= (.-key e) "Backspace")
                   (= (.-key e) "Delete"))
             {:fx [[:dispatch [::clear-area selection]]]}
             (if-let [move-to (cond
                                (= (.-key e) "ArrowUp") [(dec r) c]
                                (= (.-key e) "ArrowLeft") [r (dec c)]
                                (= (.-key e) "ArrowDown") [(if mr (inc mr) (inc r)) c]
                                (= (.-key e) "ArrowRight") [r (if mc (inc mc) (inc c))])]
               {:fx [[:dispatch [::set-selection {:start move-to :end move-to}]]]}
               (if (= (count (.-key e)) 1)
                 {:fx [[:dispatch [::edit-cell [r c] (.-key e)]]]}
                 {:fx [[:dispatch [::edit-cell [r c]]]]})))))))))

(rf/reg-event-fx
 ::paste-addressed-cells
 (undoable)
 (fn paste-addressed-cells [{:keys [db]} [_ addressed-cells]]
   (let [selection (get-in db [:ui :grid :selection])]
     {:db (update-in db [:sheet] #(grid/update-cells-bulk %
                                                          selection
                                                          addressed-cells))
      :fx [[:dispatch [::set-selection (grid/pasted-area
                                        (:start selection)
                                        (keys addressed-cells))]]]})))

(rf/reg-fx
 ::copy-to-clipboard
 (fn [[plain-text html]]
   (.write (.-clipboard js/navigator)
           [(new js/ClipboardItem
                 #js {"text/plain" (new js/Blob [plain-text] {:type "text/plain"})
                      "text/html" (new js/Blob [html] {:type "text/html"})})])))

(rf/reg-event-fx
 ::copy-selection
 (fn copy-selection [{:keys [db]}]
   (let [selection (get-in db [:ui :grid :selection])]
     {:fx [[::copy-to-clipboard
            [(paste/selection->plain-text selection (:sheet db))
             (paste/selection->html selection (:sheet db))]]]})))

(rf/reg-event-fx
 ::cut-selection
 (undoable)
 (fn cut-selection [{:keys [db]}]
   {:fx [[:dispatch [::copy-selection]]
         [:dispatch [::clear-area (get-in db [:ui :grid :selection])]]]}))

(rf/reg-event-fx
 ::merge-cells
 (undoable)
 (fn merge-cells [{:keys [db]} [_ area]]
   {:db (update-in db [:sheet] #(grid/merge-cells % area))
    :fx [[:dispatch [::edit-cell (:start area)]]]}))

(rf/reg-event-fx
 ::unmerge-cells
 (undoable)
 (fn unmerge-cells [{:keys [db]} [_ addresses]]
   {:db (update-in db [:sheet] #(grid/unmerge-cells % addresses))
    :fx [[:dispatch [::edit-cell (first addresses)]]]}))

(rf/reg-event-db
 ::set-cell-backgrounds
 (undoable)
 (fn set-cell-backgrounds [db [_ addresses background]]
   (update-in db [:sheet] #(grid/set-cell-backgrounds % addresses background))))

(rf/reg-event-db
 ::toggle-cell-bold
 (undoable)
 (fn toggle-cell-bold [db [_ addresses]]
   (update-in db [:sheet] #(grid/toggle-cell-bolds % addresses))))

(rf/reg-event-fx
 ::submit-cell-input
 (fn submit-cell-input [{:keys [db]} [_ content]]
   (let [editing-cell (get-in db [:ui :grid :editing-cell])]
     {:fx [[:dispatch [::clear-edit-cell]]
           [:dispatch [::update-cell editing-cell content]]]})))

(rf/reg-event-db
 ::resize-row
 (undoable)
 (fn resize-row [db [_ row height]]
   (assoc-in db [:sheet :grid-dimensions :row-heights row] height)))

(rf/reg-event-db
 ::resize-col
 (undoable)
 (fn resize-col [db [_ col width]]
   (assoc-in db [:sheet :grid-dimensions :col-widths col] width)))

(rf/reg-fx
 ::focus-element
 (fn [[el-id text]]
   (rc/after-render
    #(when-let [el (-> js/document (.getElementById el-id))]
       (when text (set! (.-innerHTML el) text))
       (.focus el)
       (.selectAllChildren (.getSelection js/window) el)
       (.collapseToEnd (.getSelection js/window))))))

(rf/reg-event-fx
 ::edit-cell
 (fn edit-cell [{:keys [db]} [_ rc text]]
   (let [rc* (util/merged-or-self rc (:sheet db))]
     {:db (assoc-in db [:ui :grid :editing-cell] rc*)
      :fx [[:dispatch [::set-selection {:start rc* :end (util/merged-until-or-self rc* (:sheet db))}]]
           [::focus-element ["cell-input" text]]]})))

(rf/reg-event-db
 ::clear-edit-cell
 (fn clear-edit-cell [db [_]]
   (assoc-in db [:ui :grid :editing-cell] nil)))

(rf/reg-event-db
 ::set-selection
 (fn [db [_ selection]]
   (if (util/area-inside? (:sheet db) selection)
     (assoc-in db [:ui :grid :selection] selection)
     db)))

(rf/reg-event-fx
 ::select-frame
 (fn select-frame [{:keys [db]} [_ frame-name]]
   (let [{:keys [start]} (get-in db [:sheet :frames frame-name])]
     (when start {:fx [[:dispatch [::edit-cell start]]]}))))

(rf/reg-event-fx
 ::make-frame
 (undoable)
 (fn make-frame [{:keys [db]} [_ area]]
   (let [frame-number (inc (get-in db [:sheet :last-frame-number]))
         frame-name (str "Frame " frame-number)]
     {:db (->  db
               (assoc-in [:sheet :last-frame-number] frame-number)
               (update-in [:sheet] #(frames/make-frame % frame-name area)))
      :fx [[:dispatch [::select-frame frame-name]]]})))

(rf/reg-event-db
 ::add-labels
 (undoable)
 (fn add-labels [db [_ frame-name addresses dirn]]
   (update-in db [:sheet]
              #(grid/add-frame-labels % frame-name addresses dirn))))

(rf/reg-event-db
 ::remove-labels
 (undoable)
 (fn remove-labels [db [_ frame-name addresses]]
   (->  db
        (update-in [:sheet] #(frames/unmark-skipped % frame-name addresses))
        (update-in [:sheet] #(grid/remove-frame-labels % frame-name addresses)))))

(rf/reg-event-db
 ::mark-skip-cells
 (undoable)
 (fn mark-skip-cells [db [_ frame-name addresses]]
   (update-in db [:sheet]
              #(frames/mark-skipped % frame-name addresses))))

(rf/reg-event-db
 ::explain
 (fn explain [db [_ expression]]
   (assoc-in db
             [:ui :provenance]
             (provenance/sentence-proof expression (:sheet db)))))

(rf/reg-event-db
 ::resize-frame
 (fn resize-frame [db [_ frame-name end]]
   (let [start (get-in (:sheet db) [:frames frame-name :start])]
     (when (and (not= (get-in (:sheet db) [:frames frame-name :end]) end)
                (>= (first end) (first start))
                (>= (second end) (second start)))
       (update-in db [:sheet]
                  #(grid/resize-frame % frame-name {:start start :end end}))))))

(rf/reg-event-db
 ::display-help
 (fn display-help [db [_ flag]]
   (assoc-in db [:ui :help-display] flag)))

(rf/reg-event-db
 ::set-route
 (fn set-route [db [_ match]]
   (assoc-in db [:route] match)))
