(ns bean.ui.drawing
  (:require [bean.ui.util :refer [px]]
            [re-frame.core :as rf]
            [bean.ui.subs :as subs]
            [bean.ui.events :as events]
            [reagent.core :as rc]))

(defn resize-top [e]
  (let [sheet (.getElementById js/document "bean-sheet")
        indicator (.getElementById js/document "bean-resize-indicator-v")
        label (.-target e)
        label-begin (.-offsetLeft label)
        col (js/parseInt (.getAttribute label "data-col"))
        sheet-begin (.-left (.getBoundingClientRect sheet))
        sheet-click-pos #(- (.-clientX %) sheet-begin)]
    (letfn [(move-indicator
              [e2]
              (set! (.-left (.-style indicator)) (px (sheet-click-pos e2))))
            (update-size
              [e2]
              (let [new-size (- (sheet-click-pos e2) label-begin)]
                (when (pos? new-size)
                  (rf/dispatch [::events/resize-col col new-size]))
                (.removeEventListener js/document "mousemove" move-indicator)
                (.removeEventListener js/document "mouseup" update-size)
                (set! (.. indicator -style -display) "none")
                (set! (.. sheet -style -cursor) "auto")))]
      (when
       (< (- (+ label-begin (.-offsetWidth label)) (sheet-click-pos e)) 10)
        (set! (.. indicator -style -display) "block")
        (set! (.. sheet -style -cursor) "ew-resize")
        (move-indicator e)
        (.addEventListener js/document "mousemove" move-indicator)
        (.addEventListener js/document "mouseup" update-size)))))

(defn resize-left [e]
  (let [sheet (.getElementById js/document "bean-sheet")
        indicator (.getElementById js/document "bean-resize-indicator-h")
        label (.-target e)
        label-begin (.-offsetTop label)
        row (js/parseInt (.getAttribute label "data-row"))
        sheet-begin (.-top (.getBoundingClientRect sheet))
        sheet-click-pos #(- (.-clientY %) sheet-begin)]
    (letfn [(move-indicator
              [e2]
              (set! (.-top (.-style indicator)) (px (sheet-click-pos e2))))
            (update-size
              [e2]
              (let [new-size (- (sheet-click-pos e2) label-begin)]
                (when (pos? new-size)
                  (rf/dispatch [::events/resize-row row new-size]))
                (.removeEventListener js/document "mousemove" move-indicator)
                (.removeEventListener js/document "mouseup" update-size)
                (set! (.. indicator -style -display) "none")
                (set! (.. sheet -style -cursor) "auto")))]
      (when (< (- (+ label-begin (.-offsetHeight label)) (sheet-click-pos e)) 8)
        (set! (.. indicator -style -display) "block")
        (set! (.. sheet -style -cursor) "ns-resize")
        (move-indicator e)
        (.addEventListener js/document "mousemove" move-indicator)
        (.addEventListener js/document "mouseup" update-size)))))
