(ns bean.ui.styles)

(def ^:private light {:sheet-background 0xffffff
                      :heading-background 0xf2f2f1
                      :heading-color 0x555555
                      :grid-line 0x0f0f0f
                      :resizer-line 0x999999
                      :heading-border 0xdddddd
                      :corner-background 0xf2f2f1
                      :cell-color 0x000000
                      :cell-error-color 0xb93333
                      :selection 0x888888
                      :selection-alpha 0.06
                      :frame-border 0x3b5aa3
                      :frame-name 0x3b5aa3
                      :llm-icon 0x999999})

(def colors light)

(def sizes {:world-h 10000
            :world-w 10000
            :num-rows 80
            :num-cols 16
            :cell-h 30
            :cell-w 110
            :cell-padding 5
            :cell-font-size 14
            :error-font-size 9
            :heading-left-width 35
            :heading-font-size 13
            :heading-border 1
            :resizer-handle 20
            :selection-border 1.5
            :frame-border 1
            :frame-highlight 2
            :frame-name-font 12
            :frame-name-padding 3})

(def cell-background-colors
  [nil 0xcccccc 0xb2f2bb 0xa5d8ff 0xffec99])
