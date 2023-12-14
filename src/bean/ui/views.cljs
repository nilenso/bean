(ns bean.ui.views
  (:require
   [re-frame.core :as rf]
   [bean.ui.subs :as subs]
   [bean.ui.code :as code]
   [bean.ui.help :as help]
   [bean.ui.sheet :as sheet]))

(defn main-panel []
  (let [ui (rf/subscribe [::subs/ui])]
    [:div {:class [:container
                   (when (= (:help-display @ui) "block")
                     "help-open")]}
     [help/help]
     [:div {:class :sheet-container}
      [code/text-area]
      [sheet/sheet]]]))
