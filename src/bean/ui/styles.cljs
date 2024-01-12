(ns bean.ui.styles)

(def ^:private light {:sheet-background 0xffffff
                      :heading-background 0xf2f2f1
                      :heading-color 0x555555
                      :grid-line 0xdedbd7
                      :resizer-line 0x999999
                      :heading-border 0xcccccc
                      :corner-background 0xdddddd
                      :cell-color 0x000000
                      :cell-error-color 0xb93333})

(def colors light)

(def sizes {:world-h 10000
            :world-w 10000
            :num-rows 50
            :num-cols 20
            :cell-h 30
            :cell-w 110
            :cell-padding 5
            :cell-font-size 13
            :error-font-size 9
            :heading-left-width 40
            :heading-font-size 13
            :resizer-handle 20})
