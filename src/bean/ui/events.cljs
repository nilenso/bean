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
 ::resize-row
 (fn resize-row [db [_ row height]]
   (assoc-in db [:sheet :grid-dimensions :row-heights row] height)))

(rf/reg-event-db
 ::resize-col
 (fn resize-col [db [_ col width]]
   (assoc-in db [:sheet :grid-dimensions :col-widths col] width)))

(rf/reg-event-db
 ::set-pixi-app
 (fn init-pixi-app [db [_ app]]
   (assoc-in db [:ui :pixi :app] app)))

(rf/reg-event-fx
 ::set-pixi-viewport
 (fn init-pixi-viewport [{:keys [db]} [_ app viewport]]
   {:db (assoc-in db [:ui :pixi :viewport] viewport)
    :fx [[::setup-canvas [app viewport]]]}))

;; TODO: this canvas interaction should probably be in drawing.cljs but
;; it's also an effect. Shoud move this elsewhere.
(rf/reg-fx
 ::setup-canvas
 (fn [[app viewport]]
   (.appendChild
    (.getElementById js/document "canvas-container")
    (.-view app))
   (set! (.-__PIXI_APP__ js/globalThis) app)
   (.addEventListener js/window "wheel" #(.preventDefault %1) #js {:passive false})
   (.addChild (.-stage app) viewport)
   (-> viewport
       (.pinch)
       (.drag #js {:clampWheel true})
       (.wheel #js {:trackpadPinch true :wheelZoom false})
       (.clamp #js {:direction "all"}))))

(rf/reg-fx
 ::focus-cell
 (fn [[r c]]
   (.focus
    (.querySelector
     js/document
     (str "[data-row=\"" r "\"][data-col=\"" c "\"]")))))

(rf/reg-event-fx
 ::set-mode
 (fn set-mode [{:keys [db]} [_ [r c] mode]]
   (case mode
     :view {:db (assoc-in db [:sheet :grid r c :mode] mode)}
     :edit {:db (assoc-in db [:sheet :grid r c :mode] mode)
            :fx [[::focus-cell [r c]]]})))

(rf/reg-event-fx
 ::edit-mode
 (fn edit-mode [_ [_ [r c]]]
   {:fx [[:dispatch [::set-mode [r c] :edit]]]}))

(rf/reg-event-db
 ::start-selection
 (fn [db [_ rc]]
   (assoc-in db [:ui :selection-start] rc)))

(rf/reg-event-db
 ::finish-selection
 (fn [db [_]]
   (assoc-in db [:ui :selection-start] nil)))

(rf/reg-event-db
 ::make-selection
 (fn [db [_ selection]]
   (update-in db [:ui :selections] conj selection)))

(rf/reg-event-db
 ::clear-selections
 (fn [db [_]]
   (assoc-in db [:ui :selections] [])))

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
