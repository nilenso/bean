(ns bean.frames
  (:require [bean.area :as area]
            [bean.util :as util]
            [clojure.set :as set]))

(defn overlaps? [sheet frame-name area]
  (some
   #(area/overlap? % area)
   (vals (dissoc (:frames sheet) frame-name))))

(defn make-frame [sheet frame-name area]
  (if (and (not (area/area-empty? area))
           (not (overlaps? sheet frame-name area)))
    (assoc-in sheet [:frames frame-name]
              (merge area {:labels {}
                           :skip-cells #{}}))
    sheet))

(defn cell-frame [[r c] sheet]
  (some
   (fn [[frame-name {:keys [start end]}]]
     (let [[start-r start-c] start
           [end-r end-c] end]
       (when (and (>= r start-r)
                  (<= r end-r)
                  (>= c start-c)
                  (<= c end-c))
         frame-name)))
   (:frames sheet)))

(defn add-label [sheet frame-name rc dirn & [color]]
  (if (= (cell-frame rc sheet) frame-name)
    (assoc-in
     sheet
     [:frames frame-name :labels (util/merged-or-self rc sheet)]
     {:dirn dirn :color color})
    sheet))

(defn add-labels [sheet frame-name addresses dirn]
  (reduce #(add-label %1 frame-name %2 dirn
                      (case dirn
                        :top (util/random-color-hex (str (first %2) dirn))
                        :left (util/random-color-hex (str (second %2) dirn))
                        (util/random-color-hex))) sheet addresses))

(defn remove-labels [sheet frame-name addresses]
  (reduce #(update-in % [:frames frame-name :labels] dissoc %2) sheet addresses))

(defn get-frame [sheet frame-name]
  (get-in sheet [:frames frame-name]))

(defn- get-label [sheet frame-name rc & [dirn]]
  (let [label (get-in sheet [:frames frame-name :labels rc])]
    (if dirn
      (when (= (:dirn label) dirn)
        label)
      label)))

(defn label? [sheet frame-name label-name & [dirn]]
  (some
   (fn [[label label-data]]
     (and (= label-name
             (str (:scalar (util/get-cell (:grid sheet) label))))
          (if dirn (= (:dirn label-data) dirn) true)))
   (:labels (get-frame sheet frame-name))))

(defn merge-labels [sheet start addresses]
  (if-let [frame-name (cell-frame start sheet)]
    (let [is-label? (get-label sheet frame-name start)
          other-labels? (and (not is-label?)
                             (some #(get-label sheet frame-name %) addresses))
          label (or is-label? other-labels?)]
      (if label
        (-> sheet
            (remove-labels frame-name addresses)
            (add-label frame-name start (:dirn label) (:color label)))
        sheet))
    sheet))

(defn- last-row [[r c] sheet]
  (+ r (dec (area/cell-h sheet [r c]))))

(defn- last-col [[r c] sheet]
  (+ c (dec (area/cell-w sheet [r c]))))

(defn- left-blocking-label [sheet [r c] labels]
  (some
   (fn [[[r* c*] {:keys [dirn]}]]
     (when
      (and
       (= dirn :left)
       (= r r*)
       (= (area/cell-h sheet [r c])
          (area/cell-h sheet [r* c*]))
       (> c* (last-col [r c] sheet)))
       [r* c*]))
   (sort-by (fn [[[_ c] _]] c) labels)))

(defn- top-blocking-label [sheet [r c] labels]
  (some
   (fn [[[r* c*] {:keys [dirn]}]]
     (when
      (and
       (= dirn :top)
       (= c c*)
       (= (area/cell-w sheet [r c])
          (area/cell-w sheet [r* c*]))
       (> r* (last-row [r c] sheet)))
       [r* c*]))
   (sort-by (fn [[[r _] _]] r) labels)))

(defn- top-left-blocking-label [sheet [r c] labels]
  (or (some
       (fn [[[r* c*] {:keys [dirn]}]]
         (when (= dirn :top-left)
           (cond
             (and (= r* (last-row [r c] sheet))
                  (> c* (last-col [r c] sheet))) [nil (dec c*)]
             (and (= c* (last-col [r c] sheet))
                  (> r* (last-row [r c] sheet))) [(dec r*) nil]
             (and (> r* (last-row [r c] sheet))
                  (> c* (last-col [r c] sheet))) [r* c*])))
       (sort-by (fn [[[r _] _]] r) (dissoc labels [r c])))
      (top-blocking-label sheet [r c] labels)
      (left-blocking-label sheet [r c] labels)))

(defn blocking-label [sheet frame-name label]
  (let [{:keys [labels] :as frame} (get-frame sheet frame-name)
        {:keys [dirn]} (get-in frame [:labels label])]
    (case dirn
      :top (top-blocking-label sheet label labels)
      :top-left (top-left-blocking-label sheet label labels)
      :left (left-blocking-label sheet label labels))))

(defn label->cells [sheet frame-name label]
  (let [{:keys [end] :as frame} (get-frame sheet frame-name)
        [frame-end-r frame-end-c] end
        labels (:labels frame)
        merged-with-labels (mapcat
                            #(get-in (util/get-cell (:grid sheet) %)
                                     [:style :merged-addresses])
                            (keys labels))]
    (when-let [{:keys [dirn]} (get labels label)]
      (as->
       (area/area->addresses
        {:start label
         :end (let [[br bc] (blocking-label sheet frame-name label)]
                (case dirn
                  :top [(if br (dec br) frame-end-r)
                        (min (last-col label sheet) frame-end-c)]
                  :left [(min (last-row label sheet) frame-end-r)
                         (if bc (dec bc) frame-end-c)]
                  :top-left [(if br br frame-end-r)
                             (if bc bc frame-end-c)]))}) cells
        (disj cells label)
        (apply disj cells merged-with-labels)))))

(defn skipped-cells [sheet frame-name]
  (let [frame (get-frame sheet frame-name)
        labels (:labels frame)
        skip-labels (filter #(get-in frame [:skip-cells %]) (keys labels))]
    (set/union
     (set (mapcat #(label->cells sheet frame-name %) skip-labels))
     (:skip-cells frame))))

(defn label-name->cells [sheet frame-name label-name & [dirn]]
  (let [labels (->> (keys (:labels (get-frame sheet frame-name)))
                    (filter #(get-label sheet frame-name % dirn))
                    (filter #(when (= (str label-name)
                                      (str (:scalar (util/get-cell (:grid sheet) %))))
                               %)))
        skip-label? #(get-in sheet [:frames frame-name :skip-cells %])
        all-skipped-cells (skipped-cells sheet frame-name)
        label-cells (->> labels
                         (map #(do [% (label->cells sheet frame-name %)]))
                         (into {}))
        ;; we keep track of the cells that were skipped at each step separately
        ;; so if a skip label is used at any step it can still access the skipped cells
        ;; in the function chain.
        skips (->> label-cells
                   vals
                   (apply set/union)
                   (set/intersection all-skipped-cells))]
    {:cells (->> label-cells
                 (map
                  (fn [[label cells]]
                    (if (skip-label? label)
                      cells
                      (set/difference cells skips))))
                 (apply set/union))
     :skips skips}))

(defn mark-skipped [sheet frame-name addresses]
  (update-in sheet [:frames frame-name :skip-cells] #(apply conj % (set addresses))))

(defn unmark-skipped [sheet frame-name addresses]
  (let [addresses*
        (set/union
         (set addresses)
         (set (mapcat #(label->cells sheet frame-name %) addresses)))]
    (update-in sheet [:frames frame-name :skip-cells] #(apply disj % addresses*))))

(defn- remove-outside-labels [sheet frame-name]
  (let [labels (get-in sheet [:frames frame-name :labels])
        {:keys [start end]} (get-in sheet [:frames frame-name])]
    (reduce
     #(if-not (area/overlap?
               {:start start :end end}
               {:start %2 :end %2})
        (update-in %1 [:frames frame-name :labels] dissoc %2)
        %1) sheet
     (keys labels))))

(defn resize-frame [sheet frame-name area]
  (when-not (overlaps? sheet frame-name area)
    (-> (update-in sheet [:frames frame-name] merge area)
        (remove-outside-labels frame-name))))

(defn- move-labels [sheet frame-name move-from move-to]
  (assoc-in
   sheet [:frames frame-name :labels]
   (update-keys
    (get-in sheet [:frames frame-name :labels])
    #(util/offset move-to (util/distance move-from %)))))

(defn- move-skip-cells [sheet frame-name move-from move-to]
  (assoc-in
   sheet [:frames frame-name :skip-cells]
   (map
    #(util/offset move-to (util/distance move-from %))
    (get-in sheet [:frames frame-name :skip-cells]))))

(defn move-frame [sheet frame-name area]
  (let [start (get-in sheet [:frames frame-name :start])]
    (-> (update-in sheet [:frames frame-name] merge area)
        (move-labels frame-name start (:start area))
        (move-skip-cells frame-name start (:start area))
        (remove-outside-labels frame-name))))

(defn expand-frames [sheet [updated-r updated-c]]
  (if-let [at-end-of-frame (some (fn [[frame-name {:keys [start end]}]]
                                   (when (and (= updated-r (inc (first end)))
                                              (< updated-c (inc (second end)))
                                              (>= updated-c (second start)))
                                     frame-name)) (:frames sheet))]
    (let [start (:start (get-frame sheet at-end-of-frame))
          [end-r end-c] (:end (get-frame sheet at-end-of-frame))]
      (or (resize-frame sheet at-end-of-frame {:start start
                                               :end [(inc end-r) end-c]})
          sheet))
    sheet))
