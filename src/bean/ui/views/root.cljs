(ns bean.ui.views.root
  (:require
   [re-frame.core :as rf]
   [bean.ui.subs :as subs]
   [bean.ui.views.code :as code]
   [bean.ui.views.help :as help]
   [bean.ui.views.sheet :as sheet]))

(defn root-page []
  (let [ui (rf/subscribe [::subs/ui])]
    [:div {:class [:container
                   (when (= (:help-display @ui) "block")
                     "help-open")]}
     [help/help]
     [:div {:class :sheet-container}
      [code/text-area]
      [sheet/sheet]]]))

(defn routed []
  (let [route (rf/subscribe [::subs/route])]
    (case (:handler @route)
      :root [root-page])))