(ns bean.ui.events
  (:require [bean.code :as code]
            [bean.code-errors :as code-errors]
            [bean.frames :as frames]
            [bean.grid :as grid]
            [bean.ui.db :as db]
            [bean.ui.demos :as demos]
            [bean.ui.interceptors :refer [savepoint]]
            [bean.ui.llm :as llm]
            [bean.ui.paste :as paste]
            [bean.ui.provenance :as provenance]
            [bean.ui.util :as util]
            [day8.re-frame.undo :as undo :refer [undoable]]
            [re-frame.core :as rf]
            [reagent.core :as rc]))

(rf/reg-event-db
 ::initialize-db
 (fn [_ [_ init-sheet]]
   (update-in (db/initial-app-db) [:sheet] merge init-sheet)))

(rf/reg-event-db
 ::update-code
 (fn update-code [db [_ code]]
   (update-in db [:sheet] #(-> %
                               (code/set-code code)
                               (assoc :code-evaluation-state :pending)))))

(rf/reg-event-db
 ::evaluate-code
 (savepoint)
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
 [(undoable)
  (savepoint)]
 (fn update-cell [db [_ address content]]
   (update-in db [:sheet] #(grid/update-cell-content address % content))))

(rf/reg-event-db
 ::clear-area
 [(undoable)
  (savepoint)]
 (fn clear-area [db [_ area]]
   (update-in db [:sheet] #(grid/clear-area % area))))

(rf/reg-event-fx
 ::save-to-slot
 ;;  for repl usage only
 (fn set-demo [{:keys [db]} [_ frame-name]]
   {:db (assoc-in db [:ui :current-demo-name] frame-name)
    :fx [[:dispatch [::export-demos]]]}))

(rf/reg-event-fx
 ::export-demos
 (fn [{:keys [db]} [_]]
   (.then
    (demos/get-demos)
    #(demos/download-edn-as-file
      (if-let [current-demo-name (get-in db [:ui :current-demo-name])]
        (assoc % current-demo-name (select-keys (:sheet db) [:grid
                                                             :depgraph
                                                             :frames
                                                             :last-frame-number
                                                             :grid-dimensions
                                                             :code-in-editor]))
        %)))
   {}))

(rf/reg-event-fx
 ::reset-demos
 [(undoable)
  (savepoint)]
 (fn [{:keys [db]} _]
  ;; This is a bit of a hammer, should be able to reset better.
   {:fx [[:dispatch [::initialize-db]]
         [:dispatch [::load-demo-names (get-in db [:ui :demo-names])]]]}))

(rf/reg-event-db
 ::load-demo
 [(undoable)
  (savepoint)]
 (fn [db [_ demo-name demo]]
   (-> db
       (assoc-in [:ui :current-demo-name] demo-name)
       (update-in [:sheet] merge demo))))

(rf/reg-event-fx
 ::select-demo
 (fn [_ [_ demo-name]]
   (.then (demos/get-demo demo-name)
          #(rf/dispatch [::load-demo demo-name %]))
   {}))

(rf/reg-event-db
 ::load-demo-names
 (fn [db [_ demo-names]]
   (assoc-in db [:ui :demo-names] demo-names)))

(rf/reg-event-db
 ::fetch-demos
 (fn [db []]
   (-> (demos/fetch-demos)
       (.then #(demos/get-demos))
       (.then #(rf/dispatch [::load-demo-names (keys %)])))
   db))

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

       (and (= (.-key e) "e") (or (.-ctrlKey e) (.-metaKey e)))
       {:fx [[:dispatch [::export-demos]]]}

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
 [(undoable)
  (savepoint)]
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
 (fn [{:keys [plain-text html]}]
   (.write (.-clipboard js/navigator)
           [(new js/ClipboardItem
                 #js {"text/plain" (new js/Blob [plain-text] {:type "text/plain"})
                      "text/html" (new js/Blob [html] {:type "text/html"})})])))

(rf/reg-event-fx
 ::copy-selection
 (fn copy-selection [{:keys [db]}]
   (let [selection (get-in db [:ui :grid :selection])]
     {:fx [[::copy-to-clipboard
            {:plain-text (paste/selection->plain-text selection (:sheet db))
             :html (paste/selection->html selection (:sheet db))}]]})))

(rf/reg-event-fx
 ::cut-selection
 [(undoable)
  (savepoint)]
 (fn cut-selection [{:keys [db]}]
   {:fx [[:dispatch [::copy-selection]]
         [:dispatch [::clear-area (get-in db [:ui :grid :selection])]]]}))

(rf/reg-event-fx
 ::merge-cells
 [(undoable)
  (savepoint)]
 (fn merge-cells [{:keys [db]} [_ area]]
   {:db (update-in db [:sheet] #(grid/merge-cells % area))
    :fx [[:dispatch [::edit-cell (:start area)]]]}))

(rf/reg-event-fx
 ::unmerge-cells
 [(undoable)
  (savepoint)]
 (fn unmerge-cells [{:keys [db]} [_ addresses]]
   {:db (update-in db [:sheet] #(grid/unmerge-cells % addresses))
    :fx [[:dispatch [::edit-cell (first addresses)]]]}))

(rf/reg-event-db
 ::set-cell-backgrounds
 [(undoable)
  (savepoint)]
 (fn set-cell-backgrounds [db [_ addresses background]]
   (update-in db [:sheet] #(grid/set-cell-backgrounds % addresses background))))

(rf/reg-event-db
 ::toggle-cell-bold
 [(undoable)
  (savepoint)]
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
 [(undoable)
  (savepoint)]
 (fn resize-row [db [_ row height]]
   (assoc-in db [:sheet :grid-dimensions :row-heights row] height)))

(rf/reg-event-db
 ::resize-col
 [(undoable)
  (savepoint)]
 (fn resize-col [db [_ col width]]
   (assoc-in db [:sheet :grid-dimensions :col-widths col] width)))

(rf/reg-fx
 ::focus-element
 (fn [[el-id text]]
   (rc/after-render
    #(when-let [el (-> js/document (.getElementById el-id))]
       (set! (.-innerHTML el) (or text ""))
       (.focus el)
       (.selectAllChildren (.getSelection js/window) el)
       (.collapseToEnd (.getSelection js/window))))))

(rf/reg-event-fx
 ::edit-cell
 (fn edit-cell [{:keys [db]} [_ rc text]]
   (let [rc* (util/merged-or-self rc (:sheet db))]
     (when (get-in db (flatten [:sheet :grid rc*]))
       (let [content (get-in db (flatten [:sheet :grid rc* :content]))]
         {:db (assoc-in db [:ui :grid :editing-cell] rc*)
          :fx [[:dispatch [::set-selection {:start rc* :end (util/merged-until-or-self rc* (:sheet db))}]]
               [::focus-element ["cell-input" (or text content)]]]})))))

(rf/reg-event-db
 ::clear-edit-cell
 (fn clear-edit-cell [db [_]]
   (assoc-in db [:ui :grid :editing-cell] nil)))

(rf/reg-event-db
 ::set-selection
 (fn [db [_ selection]]
   (when (util/area-inside? (:sheet db) selection)
     (assoc-in db [:ui :grid :selection] selection))))

(rf/reg-event-db
 ::clear-selection
 (fn [db [_]]
   (assoc-in db [:ui :grid :selection] nil)))

(rf/reg-event-fx
 ::make-frame
 [(undoable)
  (savepoint)]
 (fn make-frame [{:keys [db]} [_ area]]
   (let [frame-number (inc (get-in db [:sheet :last-frame-number]))
         frame-name (str "Frame " frame-number)]
     {:db (->  db
               (assoc-in [:sheet :last-frame-number] frame-number)
               (update-in [:sheet] #(grid/make-frame % frame-name area)))
      :fx [[:dispatch [::select-frame frame-name]]]})))

(rf/reg-event-fx
 ::remove-frame
  [(undoable)
  (savepoint)]
 (fn remove-frame [{:keys [db]} [_ frame-name]]
   {:db (update-in db [:sheet] #(frames/remove-frame % frame-name))}))

(rf/reg-event-fx
 ::select-frame
 (fn select-table [{:keys [db]} [_ frame-name]]
   (let [area (get-in db [:sheet :frames frame-name])]
     (when area {:fx [[:dispatch [::set-selection area]]]}))))

(rf/reg-event-fx
 ::renaming-frame
 (fn renaming-frame [{:keys [db]} [_ frame-name]]
   {:db (assoc-in db [:ui :renaming-frame] frame-name)
    :fx [[:dispatch [::select-frame frame-name]]]}))

(rf/reg-event-db
 ::highlight-matrix
 (fn highlight-matrix [db [_ content]]
   (assoc-in db [:ui :grid :highlighted-cells]
             (set (mapcat identity (get-in
                                    (grid/eval-content (:sheet db) content)
                                    [:frame :selection]))))))

(rf/reg-event-db
 ::rename-frame
 [(undoable)
  (savepoint)]
 (fn edit-frame [db [_ old-name new-name]]
   (update db :sheet #(grid/rename-frame % old-name new-name))))

(rf/reg-event-db
 ::add-labels
 [(undoable)
  (savepoint)]
 (fn add-labels [db [_ frame-name addresses dirn]]
   (update-in db [:sheet]
              #(grid/add-frame-labels % frame-name addresses dirn))))

(rf/reg-event-db
 ::add-preview-labels
 (fn add-preview-labels [db [_ frame-name addresses dirn]]
   (update-in db [:sheet]
              #(frames/add-preview-labels % frame-name addresses dirn))))

(rf/reg-event-db
 ::remove-labels
 [(undoable)
  (savepoint)]
 (fn remove-labels [db [_ frame-name addresses]]
   (update-in db [:sheet] #(grid/remove-frame-labels % frame-name addresses))))

(rf/reg-event-db
 ::remove-preview-labels
 (fn remove-preview-labels [db [_ frame-name addresses]]
   (update-in db [:sheet] #(frames/remove-preview-labels % frame-name addresses))))

(rf/reg-event-db
 ::dismiss-popup
 (fn dismiss-popup [db [_ popup-type]]
   (update-in db [:ui :popups] dissoc popup-type)))

(rf/reg-event-db
 ::popup-add-labels
 (fn popup-add-labels [db [_ suggestions]]
   (-> (assoc-in db [:ui :asking-llm] false)
       (update-in [:ui :popups] assoc :add-labels suggestions))))

(rf/reg-event-fx
 ::ask-labels-llm
 (fn add-labels-llm [{:keys [db]} [_ frame-name addresses]]
   (.then (llm/suggest-labels (:sheet db) frame-name)
          #(rf/dispatch [::popup-add-labels %]))
   {:db (assoc-in db [:ui :asking-llm] true)
    :fx [[:dispatch [::dismiss-popup :add-labels]]]}))

(rf/reg-event-db
 ::mark-skip-cells
 [(undoable)
  (savepoint)]
 (fn mark-skip-cells [db [_ frame-name addresses]]
   (update-in db [:sheet]
              #(grid/mark-skip-cells % frame-name addresses))))

(rf/reg-event-db
 ::unmark-skip-cells
 [(undoable)
  (savepoint)]
 (fn unmark-skip-cells [db [_ frame-name addresses]]
   (update-in db [:sheet]
              #(grid/unmark-skip-cells % frame-name addresses))))

(rf/reg-event-db
 ::explain
 (fn explain [db [_ expression]]
   (assoc-in db
             [:ui :provenance]
             (provenance/sentence-proof expression (:sheet db)))))

(rf/reg-event-db
 ::resize-frame
 [(undoable)
  (savepoint)]
 (fn resize-frame [db [_ frame-name end]]
   (let [start (get-in (:sheet db) [:frames frame-name :start])]
     (when (and (not= (get-in (:sheet db) [:frames frame-name :end]) end)
                (>= (first end) (first start))
                (>= (second end) (second start)))
       (update-in db [:sheet]
                  #(grid/resize-frame % frame-name {:start start :end end}))))))

(rf/reg-event-db
 ::move-frame
 [(undoable)
  (savepoint)]
 (fn move-frame [db [_ frame-name move-to]]
   (update-in db [:sheet] #(grid/move-frame % frame-name move-to))))

(rf/reg-event-db
 ::display-help
 (fn display-help [db [_ flag]]
   (assoc-in db [:ui :help-display] flag)))

(rf/reg-event-db
 ::set-route
 (fn set-route [db [_ match]]
   (assoc-in db [:route] match)))

(rf/reg-event-db
 ::set-anthropic-api-key
 [(savepoint)]
 (fn [db [_ api-key]]
   (assoc-in db [:sheet :anthropic-api-key] api-key)))
