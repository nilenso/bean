(ns bean.ui.styles)

(def ^:private light {:sheet-background 0xffffff
                      :heading-background 0xf2f2f1
                      :heading-color 0x555555
                      :grid-line 0x0f0f0f
                      :resizer-line 0x999999
                      :heading-border 0xcccccc
                      :corner-background 0xdedede
                      :cell-color 0x000000
                      :cell-error-color 0xb93333
                      :selection 0x888888
                      :selection-alpha 0.06
                      :table-selection 0x4563a7})

(def colors light)

(def sizes {:world-h 10000
            :world-w 10000
            :num-rows 50
            :num-cols 20
            :cell-h 30
            :cell-w 110
            :cell-padding 5
            :cell-font-size 14
            :error-font-size 9
            :heading-left-width 40
            :heading-font-size 13
            :resizer-handle 20
            :selection-border 1.5})

(def cell-background-colors
  [nil 0xcccccc 0xffc9c9 0xb2f2bb 0xa5d8ff 0xffec99])
