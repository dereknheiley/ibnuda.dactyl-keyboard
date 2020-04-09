(ns dactyl-keyboard.lightcycle
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.common :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frows [c]
  (let [use-numrow? (get c :configuration-use-numrow?)
        use-lastrow? (get c :configuration-use-lastrow?)
        row-start (if use-numrow? 0 1)
        row-end (if use-lastrow? 5 4)]
    (range row-start row-end)))

(defn flastrow [use-lastrow?]
  (if use-lastrow? 5 4))
(defn fcornerrow [use-lastrow?]
  (if use-lastrow? 4 3))
(defn fmiddlerow [use-lastrow?]
  (if use-lastrow? 3 2))

(defn fpenultcol [ncols] (dec ncols))
(defn fantecol   [ncols] (dec (fpenultcol ncols)))

(defn fthumb-offset [c]
  (let [thumb-offset-x (get c :configuration-thumb-offset-x)
        thumb-offset-y (get c :configuration-thumb-offset-y)
        thumb-offset-z (get c :configuration-thumb-offset-z)]
    [thumb-offset-x thumb-offset-y (+ thumb-offset-z 13)]))

(defn column-offset [column]
  (cond
    (= column 2) [0 2.82 -3.0] ;;was moved -4.5
    (>= column 4) [0 -5.8 5.64]
    :else [0 0 0]))

(defn key-place [c column row shape]
  (let [alpha (get c :configuration-alpha)
        beta (get c :configuration-beta)
        tenting-angle (get c :configuration-tenting-angle)
        z-offset (get c :configuration-z-offset)
        row-placed-shape (->> shape
                              (translate [0 0 (- (frow-radius alpha))])
                              (rotate (* alpha (- 2 row)) [1 0 0])
                              (translate [0 0 (frow-radius alpha)]))
        column-angle (* beta (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- (fcolumn-radius beta))])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 (fcolumn-radius beta)])
                          (translate (column-offset column)))]
    (->> placed-shape
         (rotate tenting-angle [0 1 0])
         (translate [0 0 z-offset]))))

(defn case-place [c column row shape]
  (let [alpha (get c :configuration-alpha)
        beta (get c :configuration-beta)
        tenting-angle (get c :configuration-tenting-angle)
        z-offset (get c :configuration-z-offset)
        
        row-placed-shape (->> shape
                              (translate [0 0 (- (frow-radius alpha))])
                              (rotate (* alpha (- 2 row)) [1 0 0])
                              (translate [0 0 (frow-radius alpha)]))
        column-offset [0 -4.35 5.64]
        column-angle (* beta (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- (fcolumn-radius beta))])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 (fcolumn-radius beta)])
                          (translate column-offset))]
    (->> placed-shape
         (rotate tenting-angle [0 1 0])
         (translate [0 0 z-offset]))))

(defn key-holes [c]
  (let [ncols (get c :configuration-ncols)
        use-alps? (get c :configuration-use-alps?)
        rotation-for-keyhole (if use-alps? 0 270)
        columns (range 0 ncols)
        rows (frows c)]
  (apply union
         (for [column columns
               row rows
               :when (not (and (= column 0) (> row 3)))]
           (->> (single-plate c)
                (rotate (deg2rad rotation-for-keyhole) [0 0 1])
                (key-place c column row))))))

(defn caps [c]
  (let [ncols (get c :configuration-ncols)
        columns (range 0 ncols)
        rows (frows c)
        lastrow (flastrow (get c :configuration-use-lastrow?))]
    (apply union
           (for [column columns
                 row rows
                 :when (or (not= column 0)
                           (not= row lastrow))]
             (->> (sa-cap 1)
                  (key-place c column row))))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(defn connectors [c]
  (let [use-lastrow? (get c :configuration-use-lastrow?)
        ncols (get c :configuration-ncols)
        columns (range 0 ncols)
        rows (frows c)
        lastrow (flastrow use-lastrow?)
        cornerrow (fcornerrow use-lastrow?)]
  (apply union
         (concat
          ;; Row connections
          (for [column (drop-last columns)
                row rows
                :when (or (not= column 0)
                          (not= row lastrow))]
            (triangle-hulls
             (key-place c (inc column) row web-post-tl)
             (key-place c column row web-post-tr)
             (key-place c (inc column) row web-post-bl)
             (key-place c column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (drop-last rows)
                :when (or (not= column 0)
                          #_(not= row cornerrow)
                          (not (and (= column 0)
                                    (> row 2))))]
            (triangle-hulls
             (key-place c column row web-post-bl)
             (key-place c column row web-post-br)
             (key-place c column (inc row) web-post-tl)
             (key-place c column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (drop-last columns)
                row (drop-last rows)
                :when (or (not= column 0)
                          (not= row cornerrow))]
            (triangle-hulls
             (key-place c column row web-post-br)
             (key-place c column (inc row) web-post-tr)
             (key-place c (inc column) row web-post-bl)
             (key-place c (inc column) (inc row) web-post-tl)))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(defn thumb-place [c column row shape]
    (let [beta (get c :configuration-beta)
          alpha (get c :configuration-alpha)
          thumb-tenting-angle (get c :configuration-thumb-tenting-angle)
          thumb-offset (fthumb-offset c)
          cap-top-height (+ plate-thickness sa-profile-key-height)
          row-radius (+ (/ (/ (+ mount-height 1) 2)
                           (Math/sin (/ alpha 2)))
                        cap-top-height)
          column-radius (+ (/ (/ (+ mount-width 2) 2)
                              (Math/sin (/ beta 2)))
                           cap-top-height)
          #_(+ (/ (/ (+ pillar-width 5) 2)
                  (Math/sin (/ beta 2)))
               cap-top-height)]
      (->> shape
           (translate [0 0 (- row-radius)])
           (rotate (* alpha row) [1 0 0])
           (translate [0 0 row-radius])
           (translate [0 0 (- column-radius)])
           (rotate (* column beta) [0 1 0])
           (translate [0 0 column-radius])
           (translate [mount-width 0 0])
           (rotate (* pi (- 1/4 3/16)) [0 0 1])
           #_(rotate beta [1 1 0])
           (rotate thumb-tenting-angle [1 1 0])
           (translate thumb-offset))))

(defn thumb-2x-column [c shape]
  (thumb-place c 0 -1/2 (rotate (/ pi 1) [0 0 1] shape)))

(defn thumb-2x+1-column [c shape]
  (union (thumb-place c 1 -1/2 (rotate (/ pi 2) [0 0 1] shape))
         (thumb-place c 1 1 shape)))

(defn thumb-1x-column [c shape]
  (union (thumb-place c 2 -3/4 shape)
         (thumb-place c 2 3/4 shape)))

(def double-plates
  (let [plate-height (/ (- sa-double-length mount-height) 2)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        stabilizer-cutout (union (->> (cube 14.2 3.5 web-thickness)
                                      (translate [0.5 12 (- plate-thickness (/ web-thickness 2))])
                                      (color [1 0 0 1/2]))
                                 (->> (cube 16 3.5 web-thickness)
                                      (translate [0.5 12 (- plate-thickness (/ web-thickness 2) 1.4)])
                                      (color [1 0 0 1/2])))
        top-plate (difference top-plate stabilizer-cutout)]
    (color [1 0 0] (union top-plate (mirror [0 1 0] top-plate)))))

(defn extended-plate-height [size] (/ (- (* (+ 1 sa-length) size) mount-height) 2))

(defn extended-plates [size]
  (let [plate-height (extended-plate-height size)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (color [0 1 1] (union top-plate (mirror [0 1 0] top-plate)))))

(defn thumb-layout [c shape]
  (let [thumb-count (get c :configuration-thumb-count)]
    (union
     (thumb-place c 0 -1/2 (union shape (extended-plates 2)))
     (thumb-place c 1 -1/2 (union shape (extended-plates 2)))
     (case thumb-count
       :five (union
              (thumb-place c 1    1 (union shape (extended-plates 1)))
              (thumb-place c 2 -3/4 (union shape (extended-plates 1.5)))
              (thumb-place c 2  3/4 (union shape (extended-plates 1.5))))
       :three (thumb-place c 1    1 (union shape (extended-plates 1)))
       ()))))

(defn thumbcaps [c]
  (let [thumb-count (get c :configuration-thumb-count)]
    (union
     (thumb-2x-column c (sa-cap 2))
     (thumb-place c 1 -1/2 (sa-cap 2))
     (case thumb-count
       :two ()
       :three (thumb-place c 1 1 (sa-cap 1))
       (thumb-1x-column c (rotate (/ pi 2) [0 0 1] (sa-cap 1.5)))))))

(defn thumb-connectors [c]
  (let [thumb-count (get c :configuration-thumb-count)
        use-lastrow? (get c :configuration-use-lastrow?)
        cornerrow (fcornerrow use-lastrow?)
        thumb-tl #(->> web-post-tl
                       (translate [0 (extended-plate-height %) 0]))
        thumb-bl #(->> web-post-bl
                       (translate [0 (- (extended-plate-height %)) 0]))
        thumb-tr #(->> web-post-tr
                       (translate [0 (extended-plate-height %) 0]))
        thumb-br #(->> web-post-br
                       (translate [0 (- (extended-plate-height %)) 0]))]
    (union
      ;;Connecting the doubles
     (triangle-hulls (thumb-place c 0 -1/2 (thumb-tl 2))
                     (thumb-place c 0 -1/2 (thumb-bl 2))
                     (thumb-place c 1 -1/2 (thumb-br 2))
                     (thumb-place c 0 -1/2 (thumb-tl 2))
                     (thumb-place c 1 -1/2 (thumb-tr 2))
                     #_(thumb-place c 1  1   (thumb-br 1)))

     (case thumb-count
       :five (union
              (triangle-hulls (thumb-place c 1  1   (thumb-br 1))
                              (thumb-place c 1  1   (thumb-bl 1))
                              (thumb-place c 1 -1/2 (thumb-tr 2))
                              (thumb-place c 1 -1/2 (thumb-tl 2)))
              (triangle-hulls (thumb-place c 2 3/4 (thumb-br 1.5))
                              (thumb-place c 2 3/4 (thumb-bl 1.5))
                              (thumb-place c 2 -3/4 (thumb-tr 1.5))
                              (thumb-place c 2 -3/4 (thumb-tl 1.5)))
              (triangle-hulls (thumb-place c 2 3/4 (thumb-br 1.5))
                              (thumb-place c 2 3/4 (thumb-bl 1.5))
                              (thumb-place c 2 -3/4 (thumb-tr 1.5))
                              (thumb-place c 2 -3/4 (thumb-tl 1.5)))
              (triangle-hulls (thumb-place c 2 -3/4 (thumb-br 1.5))
                              (thumb-place c 1 -1/2 (thumb-bl 2))
                              (thumb-place c 2 -3/4 (thumb-tr 1.5))
                              (thumb-place c 1 -1/2 (thumb-tl 2))
                              (thumb-place c 2 3/4 (thumb-br 1.5))
                              (thumb-place c 1 1 (thumb-bl 1))
                              (thumb-place c 2 3/4 (thumb-tr 1.5))
                              (thumb-place c 1 7/8 (thumb-tl 1.25))))
       :three (triangle-hulls (thumb-place c 1  1   (thumb-br 1))
                              (thumb-place c 1  1   (thumb-bl 1))
                              (thumb-place c 1 -1/2 (thumb-tr 2))
                              (thumb-place c 1 -1/2 (thumb-tl 2)))
       ())

      ;;Connecting the thumb to everything
     (case thumb-count
       :two (triangle-hulls (thumb-place c 0 -1/2 (thumb-br 2))
                            (key-place   c 1 cornerrow web-post-bl)
                            (thumb-place c 0 -1/2 (thumb-tr 2))
                            (key-place   c 1    4 web-post-tl)
                            (key-place   c 1    3 web-post-bl)
                            (thumb-place c 0 -1/2 (thumb-tr 2))
                            (key-place   c 0    3 web-post-br)
                            (key-place   c 0    3 web-post-bl)
                            (thumb-place c 0 -1/2 (thumb-tr 2))
                            (thumb-place c 0 -1/2 (thumb-tl 2))
                            (key-place   c 0    3 web-post-bl)
                            (thumb-place c 1 -1/2 (thumb-tr 2))
                            (key-place   c 0    3 web-post-bl))
       (triangle-hulls (thumb-place c 0 -1/2 (thumb-br 2))
                       (key-place   c 1 cornerrow web-post-bl)
                       (thumb-place c 0 -1/2 (thumb-tr 2))
                       (key-place   c 1 4 web-post-tl)
                       (key-place   c 1 3 web-post-bl)
                       (thumb-place c 0 -1/2 (thumb-tr 2))
                       (key-place   c 0 3 web-post-br)
                       (key-place   c 0 3 web-post-bl)
                       (thumb-place c 0 -1/2 (thumb-tr 2))
                       (thumb-place c 0 -1/2 (thumb-tl 2))
                       (key-place   c 0 3 web-post-bl)
                       (thumb-place c 1 -1/2 (thumb-tr 2))
                       (thumb-place c 1  1   (thumb-br 1))
                       (key-place   c 0 3 web-post-bl)
                       (key-place   c 0 3 web-post-tl)
                       (thumb-place c 1  1 (thumb-br 1))
                       (thumb-place c 1  1 (thumb-tr 1)))))))

(defn thumb [c]
  (let [thumb-count (get c :configuration-thumb-count)]
    (union
     (thumb-layout c (rotate (/ Math/PI 2) [0 0 1] (single-plate c)))
     (color [1 0 0] (thumb-connectors c))

     (case thumb-count
       :five (union
              (thumb-place c 0 -1/2 (extended-plates 2))
              (thumb-place c 1 -1/2 double-plates))
       :three (thumb-place c 1   1  (extended-plates 1))
       ()))))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

;; In column units
(defn right-wall-column [c]
  (let [lastcol (- (get c :configuration-ncols) 1)]
    (+ lastcol 0.55)))
(def left-wall-column -1/2)
(defn thumb-back-y [c]
  (let [thumb-count (get c :configuration-thumb-count)]
    (case thumb-count :two -0.07 0.93)))
(def thumb-right-wall (- -1/2 0.05))
(def thumb-front-row (+ -1 0.07))
(defn thumb-left-wall-column [c]
  (let [thumb-count (get c :configuration-thumb-count)
        thumb-column (case thumb-count :five 5/2 3/2)]
    (+ thumb-column 0.05)))
(defn back-y [c]
  (let [rows (frows c)]
    (+ (first rows) #_0.02 -0.15)))

(defn range-inclusive [start end step]
  (concat (range start end step) [end]))

(def wall-step 0.5)
(def wall-sphere-n 5) ;;Sphere resolution, lower for faster renders

(defn wall-sphere-at [coords]
  (->> (sphere 1)
       (translate coords)
       (with-fn wall-sphere-n)))

(defn scale-to-range [start end x]
  (+ start (* (- end start) x)))

(defn wall-sphere-bottom [front-to-back-scale]
  (wall-sphere-at [0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 5.0)
                    front-to-back-scale)
                   -5]))

(defn wall-sphere-top [front-to-back-scale]
  (wall-sphere-at [0
                   (scale-to-range
                    (+ (/ mount-height -2) -3.5)
                    (+ (/ mount-height 2) 3.5)
                    front-to-back-scale)
                   5]))

(def wall-sphere-top-back (wall-sphere-top 1))
(def wall-sphere-bottom-back (wall-sphere-bottom 1))
(def wall-sphere-bottom-front (wall-sphere-bottom 0))
(def wall-sphere-top-front (wall-sphere-top 0))

(defn top-case-cover [place-fn sphere
                      x-start x-end
                      y-start y-end
                      step]
  (apply union
         (for [x (range-inclusive x-start (- x-end step) step)
               y (range-inclusive y-start (- y-end step) step)]
           (hull (place-fn x y sphere)
                 (place-fn (+ x step) y sphere)
                 (place-fn x (+ y step) sphere)
                 (place-fn (+ x step) (+ y step) sphere)))))

(defn front-wall [c]
  (let [use-lastrow? (get c :configuration-use-lastrow?)
        ncols (get c :configuration-ncols)
        lastrow (flastrow use-lastrow?)
        cornerrow (fcornerrow use-lastrow?)
        penultcol (fpenultcol ncols)
        antecol (fantecol ncols)
        step wall-step ;;0.1
        wall-step 0.1 ;;0.05
        place (partial case-place c)
        top-cover (fn [x-start x-end y-start y-end]
                    (top-case-cover place wall-sphere-top-front
                                    x-start x-end y-start y-end
                                    wall-step))]
    (union
     (apply union
            (for [x (range-inclusive 0.7 (- (right-wall-column c) step) step)]
              (hull (place x cornerrow wall-sphere-top-front)
                    (place (+ x step) cornerrow wall-sphere-top-front)
                    (place x cornerrow wall-sphere-bottom-front)
                    (place (+ x step) cornerrow wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive 0.7 (- (right-wall-column c) step) step)]
              (bottom-hull (place x cornerrow wall-sphere-bottom-front)
                           (place (+ x step) cornerrow wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive 0.5 0.7 0.1)]
              (hull (place x cornerrow wall-sphere-top-front)
                    (place (+ x step) cornerrow wall-sphere-top-front)
                    (place 0.7 cornerrow wall-sphere-bottom-front))))
     (top-cover 0.5 1.7 (* cornerrow 0.9) cornerrow)
     (top-cover 1.59 2.41 (* cornerrow 0.85) cornerrow) ;; was 3.32
     (top-cover 2.39 3.41 (* cornerrow 0.9) cornerrow)
     (apply union
            (for [x (range 2 lastrow)]
              (union
               (hull (place (- x 1/2) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
                     (place (+ x 1/2) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
                     (key-place c x cornerrow web-post-bl)
                     (key-place c x cornerrow web-post-br))
               (hull (place (- x 1/2) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
                     (key-place c x cornerrow web-post-bl)
                     (key-place c (- x 1) cornerrow web-post-br)))))
     (hull (place (right-wall-column c) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (place (- (right-wall-column c) 1) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (key-place c penultcol cornerrow web-post-bl)
           (key-place c penultcol cornerrow web-post-br))
     (hull (place (+ antecol 1/2) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (place (- (right-wall-column c) 1) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (key-place c antecol cornerrow web-post-br)
           (key-place c penultcol cornerrow web-post-bl))
     (hull (place 0.7 cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (place 1.7 cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (key-place c 1 cornerrow web-post-bl)
           (key-place c 1 cornerrow web-post-br)))))

(defn back-wall [c]
  (let [ncols (get c :configuration-ncols)
        use-lastrow? (get c :configuration-use-lastrow?)
        penultcol (fpenultcol ncols)
        antecol (fantecol ncols)
        rows (frows c)
        lastrow (flastrow use-lastrow?)
        cornerrow (flastrow use-lastrow?)
        back-row (first rows)
        step wall-step
        wall-sphere-top-backtep 0.05
        place (partial case-place c)
        front-top-cover (fn [x-start x-end y-start y-end]
                          (apply union
                                 (for [x (range-inclusive x-start (- x-end wall-sphere-top-backtep) wall-sphere-top-backtep)
                                       y (range-inclusive y-start (- y-end wall-sphere-top-backtep) wall-sphere-top-backtep)]
                                   (hull (place x y wall-sphere-top-back)
                                         (place (+ x wall-sphere-top-backtep) y wall-sphere-top-back)
                                         (place x (+ y wall-sphere-top-backtep) wall-sphere-top-back)
                                         (place (+ x wall-sphere-top-backtep) (+ y wall-sphere-top-backtep) wall-sphere-top-back)))))]
    (union
     (apply union
            (for [x (range-inclusive left-wall-column (- (right-wall-column c) step) step)]
              (hull (place x (back-y c) wall-sphere-top-back)
                    (place (+ x step) (back-y c) wall-sphere-top-back)
                    (place x (back-y c) wall-sphere-bottom-back)
                    (place (+ x step) (back-y c) wall-sphere-bottom-back))))
     (apply union
            (for [x (range-inclusive left-wall-column (- (right-wall-column c) step) step)]
              (bottom-hull (place x (back-y c) wall-sphere-bottom-back)
                           (place (+ x step) (back-y c) wall-sphere-bottom-back))))
     ;;     (front-top-cover left-wall-column 1.56 back-y (+ back-y 0.06))
     #_(front-top-cover left-wall-column right-wall-column back-y (+ back-y 0.06))
     ;;     (front-top-cover 1.56 2.44 back-y (+ back-y 0.06))
     (front-top-cover 3.56 4.44 (back-y c) (+ (back-y c) 0.2))
     (front-top-cover 4.3 (right-wall-column c) (back-y c) (+ (back-y c) 0.2))


     (hull (place left-wall-column (back-y c) (translate [1 -1 1] wall-sphere-bottom-back))
           (place (+ left-wall-column 1) (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
           (key-place c 0 back-row web-post-tl)
           (key-place c 0 back-row web-post-tr))

     (hull (place penultcol (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
           (place (right-wall-column c) (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
           (key-place c penultcol back-row web-post-tl)
           (key-place c penultcol back-row web-post-tr))

     (apply union
            (for [x (range 1 penultcol)]
              (union
               (hull (place (- x 1/2) (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
                     (place (+ x 1/2) (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
                     (key-place c x back-row web-post-tl)
                     (key-place c x back-row web-post-tr))
               (hull (place (- x 1/2) (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
                     (key-place c x back-row web-post-tl)
                     (key-place c (- x 1) back-row web-post-tr)))))
     (hull (place (- 4 1/2) (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
           (place penultcol (back-y c) (translate [0 -1 1] wall-sphere-bottom-back))
           (key-place c antecol back-row web-post-tr)
           (key-place c penultcol back-row web-post-tl)))))

(defn right-wall [c]
  (let [ncols (get c :configuration-ncols)
        use-lastrow? (get c :configuration-use-lastrow?)
        penultcol (fpenultcol ncols)
        antecol (fantecol ncols)
        rows (frows c)
        lastrow (flastrow use-lastrow?)
        cornerrow (fcornerrow use-lastrow?)
        middlerow (fmiddlerow use-lastrow?)
        wall-stop (if use-lastrow? cornerrow cornerrow)
        place (partial case-place c)]
    (union
     (apply union
            (map (partial apply hull)
                 (partition 2 1
                            (for [scale (range-inclusive 0 1 0.01)]
                              (let [x (scale-to-range wall-stop (back-y c) scale)]
                                (hull (place (right-wall-column c) x (wall-sphere-top scale))
                                      (place (right-wall-column c) x (wall-sphere-bottom scale))))))))

     (apply
      union
      (map (partial apply hull)
           (partition 2 1
                      (for [scale (range-inclusive 0 1 0.01)]
                        (let [x (scale-to-range wall-stop (back-y c) scale)]
                          (bottom-hull (place (right-wall-column c) x (wall-sphere-top scale))
                                       (place (right-wall-column c) x (wall-sphere-bottom scale))))))))

     (apply union
            (concat
             (for [x (range 1 lastrow)]
               (union
                (hull (place (right-wall-column c) x (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                      (key-place c penultcol x web-post-br)
                      (key-place c penultcol x web-post-tr))))
             (for [x (range 1 cornerrow)]
               (union
                (hull (place (right-wall-column c) x (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                      (place (right-wall-column c) (inc x) (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                      (key-place c penultcol x web-post-br)
                      (key-place c penultcol (inc x) web-post-tr))))
             [(union
               (hull (place (right-wall-column c) (first rows) (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                     (place (right-wall-column c) (back-y c) (translate [-1 -1 1] (wall-sphere-bottom 1)))
                     (key-place c penultcol (first rows) web-post-tr))
               (hull (place (right-wall-column c) cornerrow (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                     (place (right-wall-column c) cornerrow (translate [-1 1 1] (wall-sphere-bottom 0)))
                     (key-place c penultcol cornerrow web-post-br)))])))))

(defn left-wall [c]
  (let [thumb-count (get c :configuration-thumb-count)
        rows (frows c)
        place (partial case-place c)
        thumb-where (case thumb-count :two 0 1)
        finish-left-wall (case thumb-count :two 2.6666 1.6666)]
    (union
     (apply union
            (for [x (range-inclusive (dec (first rows)) (- finish-left-wall wall-step) wall-step)]
              (hull (place left-wall-column x wall-sphere-top-front)
                    (place left-wall-column (+ x wall-step) wall-sphere-top-front)
                    (place left-wall-column x wall-sphere-bottom-front)
                    (place left-wall-column (+ x wall-step) wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive (dec (first rows)) (- finish-left-wall wall-step) wall-step)]
              (bottom-hull (place left-wall-column x wall-sphere-bottom-front)
                           (place left-wall-column (+ x wall-step) wall-sphere-bottom-front))))
     (hull (place left-wall-column (dec (first rows)) wall-sphere-top-front)
           (place left-wall-column (dec (first rows)) wall-sphere-bottom-front)
           (place left-wall-column (back-y c) wall-sphere-top-back)
           (place left-wall-column (back-y c) wall-sphere-bottom-back))

     (bottom-hull (place left-wall-column (dec (first rows)) wall-sphere-bottom-front)
                  (place left-wall-column (back-y c) wall-sphere-bottom-back))
     #_(color [0 1 0] (hull (place left-wall-column 0 (translate [1 -1 1] wall-sphere-bottom-back))
                            (place left-wall-column 1 (translate [1 0 1] wall-sphere-bottom-back))
                            (key-place 0 0 web-post-tl)
                            (key-place 0 0 web-post-bl)))
     (color [0 1 0] (hull (place left-wall-column 1 (translate [1 -1 1] wall-sphere-bottom-back))
                          (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                          (key-place c 0 1 web-post-tl)
                          (key-place c 0 1 web-post-bl)))
     (color [1 0 0] (hull (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                          (place left-wall-column finish-left-wall  (translate [1 0 1] wall-sphere-bottom-front))
                          (key-place c 0 1 web-post-bl)
                          (key-place c 0 2 web-post-bl)))
     (color [1 1 0] (hull (place left-wall-column finish-left-wall  (translate [1 0 1] wall-sphere-bottom-front))
                          (key-place c 0 2 web-post-bl)
                          (key-place c 0 3 web-post-bl)
                          (key-place c 0 3 web-post-tl)))
     (hull (place left-wall-column finish-left-wall  (translate [1 0 1] wall-sphere-bottom-front))
           (thumb-place c 1 thumb-where web-post-tr)
           (key-place   c 0 3 (case thumb-count :two web-post-bl web-post-tl )))
     (hull (place left-wall-column finish-left-wall (translate [1 0 1] wall-sphere-bottom-front))
           (thumb-place c 1 thumb-where web-post-tr)
           (thumb-place c 1/2 (thumb-back-y c) (translate [0 -1 1] wall-sphere-bottom-back))))))

(defn thumb-back-wall [c]
  (let [thumb-count (get c :configuration-thumb-count)
        step wall-step
        top-step 0.05
        front-top-cover (fn [x-start x-end y-start y-end]
                          (apply union
                                 (for [x (range-inclusive x-start (- x-end top-step) top-step)
                                       y (range-inclusive y-start (- y-end top-step) top-step)]
                                   (hull (thumb-place c x y wall-sphere-top-back)
                                         (thumb-place c (+ x top-step) y wall-sphere-top-back)
                                         (thumb-place c x (+ y top-step) wall-sphere-top-back)
                                         (thumb-place c (+ x top-step) (+ y top-step) wall-sphere-top-back)))))
        local-back-y (thumb-back-y c)
        thumb-range (case thumb-count :five 5/2 3/2)
        back-thumb-position (case thumb-count :two 0 1)
        thumb-back-to-left-wall-position (case thumb-count :two 2.6666 1.6666)]
    (union
     (apply union
            (for [x (range-inclusive 1/2 (- (+ thumb-range 0.05) step) step)]
              (hull (thumb-place c x local-back-y wall-sphere-top-back)
                    (thumb-place c (+ x step) local-back-y wall-sphere-top-back)
                    (thumb-place c x local-back-y wall-sphere-bottom-back)
                    (thumb-place c (+ x step) local-back-y wall-sphere-bottom-back))))
     (apply union
            (for [x (range-inclusive 1/2 (- (+ thumb-range 0.05) step) step)]
              (bottom-hull (thumb-place c x local-back-y wall-sphere-bottom-back)
                           (thumb-place c (+ x step) local-back-y wall-sphere-bottom-back))))
     (hull (thumb-place c 1/2 local-back-y wall-sphere-top-back)
           (thumb-place c 1/2 local-back-y wall-sphere-bottom-back)
           (case-place  c left-wall-column thumb-back-to-left-wall-position wall-sphere-top-front))
     (bottom-hull (thumb-place c 1/2 local-back-y wall-sphere-bottom-back)
                  (case-place  c left-wall-column (case thumb-count :two 2.6666 1.7) wall-sphere-bottom-front))
     (hull (thumb-place c 1/2 local-back-y wall-sphere-bottom-back)
           (case-place  c left-wall-column thumb-back-to-left-wall-position wall-sphere-top-front)
           (case-place  c left-wall-column thumb-back-to-left-wall-position wall-sphere-bottom-front))
     (hull
      (thumb-place c 1/2 (thumb-back-y c) wall-sphere-bottom-back)
      (thumb-place c 1 back-thumb-position web-post-tr)
      (thumb-place c 3/2 (thumb-back-y c) wall-sphere-bottom-back)
      (thumb-place c 1 back-thumb-position web-post-tl))
     (hull
      (thumb-place c (+ 3/2 0.05) (thumb-back-y c) wall-sphere-bottom-back)
      (thumb-place c 3/2 (thumb-back-y c) wall-sphere-bottom-back)
      (thumb-place c 1 back-thumb-position web-post-tl)
      (thumb-place c 1 back-thumb-position web-post-tl)))))

(defn thumb-left-wall [c]
  (let [thumb-count (get c :configuration-thumb-count)
        step wall-step
        place (partial thumb-place c)
        column (case thumb-count :five 2 1)
        left-wall-length (case thumb-count :two 0.99 1.95)]
    (union
     (apply union
            (for [x (range-inclusive (+ -1 0.07) (- left-wall-length step) step)]
              (hull (place (thumb-left-wall-column c) x wall-sphere-top-front)
                    (place (thumb-left-wall-column c) (+ x step) wall-sphere-top-front)
                    (place (thumb-left-wall-column c) x wall-sphere-bottom-front)
                    (place (thumb-left-wall-column c) (+ x step) wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive (+ -1 0.07) (- left-wall-length step) step)]
              (bottom-hull (place (thumb-left-wall-column c) x wall-sphere-bottom-front)
                           (place (thumb-left-wall-column c) (+ x step) wall-sphere-bottom-front))))
     (case thumb-count
       :two ()
       (union (hull (place (thumb-left-wall-column c) 1.95 wall-sphere-top-front)
                    (place (thumb-left-wall-column c) 1.95 wall-sphere-bottom-front)
                    (place (thumb-left-wall-column c) (thumb-back-y c) wall-sphere-top-back)
                    (place (thumb-left-wall-column c) (thumb-back-y c) wall-sphere-bottom-back))
              (hull (place (thumb-left-wall-column c) (thumb-back-y c) (translate [1 -1 1] wall-sphere-bottom-back))
                    (place (thumb-left-wall-column c) 0 (translate [1 0 1] wall-sphere-bottom-back))
                    (place column 1 web-post-tl)
                    (place column 1 web-post-bl))
              (hull (place (thumb-left-wall-column c) 0 (translate [1 0 1] wall-sphere-bottom-back))
                    (place column 0 web-post-tl)
                    (place column 1 web-post-bl))))
     (hull
      (place (thumb-left-wall-column c) -0.1 (translate [1 0 1] wall-sphere-bottom-back))
      (place (thumb-left-wall-column c) -1   (translate [1 0 1] wall-sphere-bottom-back))
      (place column 0 web-post-tl)
      (place column 0 web-post-bl))
     (hull
      (place (thumb-left-wall-column c) -1 (translate [1 0 1] wall-sphere-bottom-back))
      (place column -1 web-post-tl)
      (place column 0 web-post-bl))
     (hull
      (place (thumb-left-wall-column c) -1 (translate [1 0 1] wall-sphere-bottom-back))
      (place (thumb-left-wall-column c) (+ -1 0.07) (translate [1 1 1] wall-sphere-bottom-front))
      (place column -1 web-post-tl)
      (place column -1 web-post-bl)))))

(defn thumb-front-wall [c]
  (let [thumb-count (get c :configuration-thumb-count)
        use-lastrow? (get c :configuration-use-lastrow?)
        cornerrow (fcornerrow use-lastrow?)
        step wall-step ;;0.1
        wall-sphere-top-fronttep 0.05 ;;0.05
        place (partial thumb-place c)
        plate-height (/ (- sa-double-length mount-height) 2)
        thumb-tl (->> web-post-tl
                      (translate [0 plate-height 0]))
        thumb-bl (->> web-post-bl
                      (translate [0 (- plate-height) 0]))
        thumb-tr (->> web-post-tr
                      (translate [-0 plate-height 0]))
        thumb-br (->> web-post-br
                      (translate [-0 (- plate-height) 0]))
        thumb-range (case thumb-count :five 5/2 3/2)]
    (union
     (apply union
            (for [x (range-inclusive thumb-right-wall (- (+ thumb-range 0.05) step) step)]
              (hull (place x thumb-front-row wall-sphere-top-front)
                    (place (+ x step) thumb-front-row wall-sphere-top-front)
                    (place x thumb-front-row wall-sphere-bottom-front)
                    (place (+ x step) thumb-front-row wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive thumb-right-wall (- (+ thumb-range 0.05) step) step)]
              (bottom-hull (place x thumb-front-row wall-sphere-bottom-front)
                           (place (+ x step) thumb-front-row wall-sphere-bottom-front))))

     (hull (place thumb-right-wall thumb-front-row wall-sphere-top-front)
           (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
           (case-place c 0.5 cornerrow wall-sphere-top-front))
     (hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
                  (case-place c 0.5 cornerrow wall-sphere-top-front))
     (bottom-hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
                  (case-place c 0.7 cornerrow wall-sphere-bottom-front))
     (hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
           (case-place c 0.5 cornerrow wall-sphere-top-front)
           (case-place c 0.7 cornerrow wall-sphere-bottom-front))

     (hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
           (key-place c 1 cornerrow web-post-bl)
           (place 0 -1/2 thumb-br)
           (place 0 -1/2 web-post-br)
           (case-place c 0.7 cornerrow wall-sphere-bottom-front))

     (hull (place (+ 3/2 0.05) thumb-front-row (translate [1 1 1] wall-sphere-bottom-front))
           (place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
           #_(place 2 -1 web-post-bl)
           #_(place 2 -1 web-post-br))

     (hull (place thumb-right-wall thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
           (place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
           (place 0 -1/2 thumb-bl)
           (place 0 -1/2 thumb-br))
     (hull (place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
           (place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
           (place 0 -1/2 thumb-bl)
           (place 1 -1/2 thumb-bl)
           (place 1 -1/2 thumb-br)
           (place 2 -1 web-post-br)))))

(defn frj9-start [c]
  (let [use-numrow? (get c :configuration-use-numrow?)]
    [-25 (if use-numrow? 55 35) 0]))

(defn fusb-holder-position [c]
  (let [use-numrow? (get c :configuration-use-numrow?)]
    [-10 (if use-numrow? 55 35) 0]))


(defn screw-insert
  "Places screw insert to its place.
   TODO: write me."
  [c column row bottom-radius top-radius height]
  ())

(defn screw-placement
  "Places all screw inserts to their places.
   TODO: write me."
  [c bottom-radius top-radius height]
  ())

(defn new-case [c]
  (difference
   (union (front-wall c)
          (right-wall c)
          (back-wall c)
          (left-wall c)
          (thumb-back-wall c)
          (thumb-left-wall c)
          (thumb-front-wall c)
          (usb-holder fusb-holder-position c))
   (rj9-space frj9-start c)))

;;;;;;;;;;;;;;;;
;;Final Export ;;
;;;;;;;;;;;;;;;;;;

(defn dactyl-top-right [c]
  (difference
   (union (key-holes c)
          (connectors c)
          (thumb c)
          (new-case c)
          (rj9-holder frj9-start c)
          #_(if (get c :configuration-show-caps?) (caps c) ())
          #_(if (get c :configuration-show-caps?) (thumbcaps c) ()))
   (usb-holder-hole fusb-holder-position c)))

(defn dactyl-top-left [c]
  (mirror [-1 0 0] (dactyl-top-right c)))

(def c
  {:configuration-ncols 5
   :configuration-use-numrow? false
   :configuration-use-lastrow? false
   :configuration-create-side-nub? false
   :configuration-use-alps? true
   :configuration-use-hotswap? true
   :configuration-thumb-count :two
   :configuration-alpha (/ pi 12)
   :configuration-beta (/ pi 36)
   :configuration-z-offset 18
   :configuration-tenting-angle (/ pi 7)
   :configuration-thumb-tenting-angle (/ pi 24)
   :configuration-thumb-offset-x -48
   :configuration-thumb-offset-y -45
   :configuration-thumb-offset-z 27
   :configuration-show-caps? false})

(spit "things/lightcycle-cherry-top-right.scad"
      (write-scad (dactyl-top-right c)))
