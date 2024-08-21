(ns bean.ui.views.root
  (:require [bean.ui.subs :as subs]
            [bean.ui.views.help :as help]
            [bean.ui.views.popups :as popups]
            [bean.ui.views.sheet :as sheet]
            [bean.ui.views.sidebar :as sidebar]
            [re-frame.core :as rf]))

(defn root-page []
  (let [ui (rf/subscribe [::subs/ui])]
    [:div {:class (when (= (:help-display @ui) "block")
                    "help-open")}
     [help/help]
     [:div {:class :container}
      [sidebar/sidebar]
      [sheet/sheet]
      [popups/popups]]]))

(defn routed []
  (let [route (rf/subscribe [::subs/route])]
    (case (:handler @route)
      :root [root-page]
      [root-page])))
