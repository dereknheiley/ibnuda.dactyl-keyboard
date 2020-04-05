(ns dactyl-keyboard.lightcycle
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (/ height 2)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14) ;; Was 14.1, then 14.25
(def keyswitch-width 14)

(def sa-profile-key-height 12.7)

(def plate-thickness 5)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ pi 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 plate-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ plate-thickness 2)]))))
        plate-half (union top-wall left-wall #_(with-fn 100 side-nub))]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

#_(def alps-width 15.6)
#_(def alps-notch-width 15.5)
#_(def alps-notch-height 1)
#_(def alps-height 13)

#_(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 2.2 plate-thickness)
                      (translate [0
                                  (+ (/ 2.2 2) (/ alps-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (union (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                              (translate [(+ (/ 1.5 2) (/ 15.6 2))
                                          0
                                          (/ plate-thickness 2)]))
                         (->> (cube 1.5 (+ keyswitch-height 3) 1.0)
                              (translate [(+ (/ 1.5 2) (/ alps-notch-width 2))
                                          0
                                          (- plate-thickness
                                             (/ alps-notch-height 2))]))
                         )
        plate-half (union top-wall left-wall)]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))


;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def thumb-count :three) ; or :five

(def nrows 4)
(def ncols 5)

(def lastrow nrows)
(def cornerrow (dec lastrow))
(def middlerow (dec cornerrow))

(def penultcol (dec ncols))
(def antecol (dec penultcol))

(def columns (range 0 ncols))
(def rows (range 1 nrows))

(def alpha (/ pi 12))
(def beta (/ pi 36))

(def column-rotation (/ pi 9))

(defn column-offset [column]
  (cond
    (= column 2) [0 2.82 -3.0] ;;was moved -4.5
    (>= column 4) [0 -5.8 5.64]
    :else [0 0 0]))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height 1/2) 2)
                      (Math/sin (/ alpha 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width 2.0) 2)
                         (Math/sin (/ beta 2)))
                      cap-top-height))

(defn key-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* alpha (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-angle (* beta (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate (column-offset column)))]
    (->> placed-shape
         (rotate column-rotation [0 1 0])
         (translate [0 0 13]))))

(defn case-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* alpha (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset [0 -4.35 5.64]
        column-angle (* beta (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-offset))]
    (->> placed-shape
         (rotate column-rotation [0 1 0])
         (translate [0 0 13]))))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (not (and (= column 0) (> row 3)))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (not= column 0)
                         (not= row lastrow))]
           (->> (sa-cap 1)
                (key-place column row)))))

(def wall-thickness 2)
(def wall-z-offset 0)
(def wall-xy-offset 0)

(defn wall-locate1 [dx dy]
  [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy]
  [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy]
  [(* dx (+ wall-xy-offset wall-thickness))
   (* dy (+ wall-xy-offset wall-thickness))
   wall-z-offset])

; if you want to change the wall, use this.
; place1 means the location at the keyboard, marked by key-place or thumb-xx-place
; dx1 means the movement from place1 in x coordinate, multiplied by wall-xy-locate.
; dy1 means the movement from place1 in y coordinate, multiplied by wall-xy-locate.
; post1 means the position this wall attached to place1.
;       xxxxx-br means bottom right of the place1.
;       xxxxx-bl means bottom left of the place1.
;       xxxxx-tr means top right of the place1.
;       xxxxx-tl means top left of the place1.
; place2 means the location at the keyboard, marked by key-place or thumb-xx-place
; dx2 means the movement from place2 in x coordinate, multiplied by wall-xy-locate.
; dy2 means the movement from place2 in y coordinate, multiplied by wall-xy-locate.
; post2 means the position this wall attached to place2.
;       xxxxx-br means bottom right of the place2.
;       xxxxx-bl means bottom left of the place2.
;       xxxxx-tr means top right of the place2.
;       xxxxx-tl means top left of the place2.
(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))
;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (drop-last columns)
                row rows
                :when (or (not= column 0)
                          (not= row lastrow))]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (drop-last rows)
                :when (or (not= column 0)
                          #_(not= row cornerrow)
                          (not (and (= column 0)
                                    (> row 2))))]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (drop-last columns)
                row (drop-last rows)
                :when (or (not= column 0)
                          (not= row cornerrow))]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(defn thumb-place [column row shape]
  (let [cap-top-height (+ plate-thickness sa-profile-key-height)
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
         (rotate beta [1 1 0])
         (translate [-52 -45 40]))))

(defn thumb-2x-column [shape]
  (thumb-place 0 -1/2 (rotate (/ pi 1) [0 0 1] shape)))

(defn thumb-2x+1-column [shape]
  (union (thumb-place 1 -1/2 (rotate (/ pi 2) [0 0 1] shape))
         (thumb-place 1 1 shape)))

(defn thumb-1x-column [shape]
  (union (thumb-place 2 -1 shape)
         (thumb-place 2 0 shape)
         (thumb-place 2 1 shape)))


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

(defn thumb-layout [shape]
  (union
   (thumb-place 0 -1/2 (union shape (extended-plates 2)))

   (thumb-place 1    1 (union shape (extended-plates 1)))
   (thumb-place 1 -1/2 (union shape (extended-plates 2)))

   (case thumb-count
     :five (union
            (thumb-place 2 -3/4 (union shape (extended-plates 1.5)))
            (thumb-place 2 3/4 (union shape (extended-plates 1.5))))
     ())))

(defn thumb-layout-bottom [shape]
  (union
   (thumb-place 0 -1/2 shape)

   (thumb-place 1 7/8 shape)
   (thumb-place 1 -5/8 shape)

   (thumb-place 2 -3/4 shape)
   (thumb-place 2 3/4 shape)))

(def thumbcaps
  (union
   (thumb-2x-column (sa-cap 2))
   (thumb-place 1 -1/2 (sa-cap 2))
   (thumb-place 1 1 (sa-cap 1))
   #_(thumb-1x-column (sa-cap 1))))

(def thumb-connectors
  (union
   #_(apply union
            (concat
             (for [column [2] row [1]]
               (triangle-hulls (thumb-place column row web-post-br)
                               (thumb-place column row web-post-tr)
                               (thumb-place (dec column) row web-post-bl)
                               (thumb-place (dec column) row web-post-tl)))
             (for [column [2] row [0 1]]
               (triangle-hulls
                (thumb-place column row web-post-bl)
                (thumb-place column row web-post-br)
                (thumb-place column (dec row) web-post-tl)
                (thumb-place column (dec row) web-post-tr)))))
   (let [thumb-tl #(->> web-post-tl
                        (translate [0 (extended-plate-height %) 0]))
         thumb-bl #(->> web-post-bl
                        (translate [0 (- (extended-plate-height %)) 0]))
         thumb-tr #(->> web-post-tr
                        (translate [0 (extended-plate-height %) 0]))
         thumb-br #(->> web-post-br
                        (translate [0 (- (extended-plate-height %)) 0]))]
     (union

      ;;Connecting the doubles
      (triangle-hulls (thumb-place 0 -1/2 (thumb-tl 2))
                      (thumb-place 0 -1/2 (thumb-bl 2))
                      (thumb-place 1 -1/2 (thumb-br 2))
                      (thumb-place 0 -1/2 (thumb-tl 2))
                      (thumb-place 1 -1/2 (thumb-tr 2))
                      #_(thumb-place 1  1   (thumb-br 1)))

      (triangle-hulls (thumb-place 1  1   (thumb-br 1))
                      (thumb-place 1  1   (thumb-bl 1))
                      (thumb-place 1 -1/2 (thumb-tr 2))
                      (thumb-place 1 -1/2 (thumb-tl 2)))

      (case thumb-count
        :five (union
               (triangle-hulls (thumb-place 2 3/4 (thumb-br 1.5))
                               (thumb-place 2 3/4 (thumb-bl 1.5))
                               (thumb-place 2 -3/4 (thumb-tr 1.5))
                               (thumb-place 2 -3/4 (thumb-tl 1.5)))
               (triangle-hulls (thumb-place 2 3/4 (thumb-br 1.5))
                               (thumb-place 2 3/4 (thumb-bl 1.5))
                               (thumb-place 2 -3/4 (thumb-tr 1.5))
                               (thumb-place 2 -3/4 (thumb-tl 1.5)))
               (triangle-hulls (thumb-place 2 -3/4 (thumb-br 1.5))
                               (thumb-place 1 -1/2 (thumb-bl 2))
                               (thumb-place 2 -3/4 (thumb-tr 1.5))
                               (thumb-place 1 -1/2 (thumb-tl 2))
                               (thumb-place 2 3/4 (thumb-br 1.5))
                               (thumb-place 1 1 (thumb-bl 1))
                               (thumb-place 2 3/4 (thumb-tr 1.5))
                               (thumb-place 1 7/8 (thumb-tl 1.25))))
        ())




      ;;Connecting the thumb to everything
      (triangle-hulls (thumb-place 0 -1/2 (thumb-br 2))
                      (key-place 1 cornerrow web-post-bl)
                      (thumb-place 0 -1/2 (thumb-tr 2))
                      (key-place 1 4 web-post-tl)
                      (key-place 1 3 web-post-bl)
                      (thumb-place 0 -1/2 (thumb-tr 2))
                      (key-place 0 3 web-post-br)
                      (key-place 0 3 web-post-bl)
                      (thumb-place 0 -1/2 (thumb-tr 2))
                      (thumb-place 0 -1/2 (thumb-tl 2))
                      (key-place 0 3 web-post-bl)
                      (thumb-place 1 -1/2 (thumb-tr 2))
                      (thumb-place 1  1   (thumb-br 1))
                      (key-place 0 3 web-post-bl)
                      (key-place 0 3 web-post-tl)
                      (thumb-place 1  1 (thumb-br 1))
                      (thumb-place 1  1 (thumb-tr 1)))))))

(def thumb
  (union
   (thumb-layout (rotate (/ Math/PI 2) [0 0 1] single-plate))
   (color [1 0 0] thumb-connectors)

   (case thumb-count
     :five (union
            (thumb-place 0 -1/2 (extended-plates 2))
            (thumb-place 1 -1/2 double-plates))
     ())))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

;; In column units
(def right-wall-column (+ (last columns) 0.55))
(def left-wall-column (- (first columns) 1/2))
(def thumb-back-y 0.93)
(def thumb-right-wall (- -1/2 0.05))
(def thumb-front-row (+ -1 0.07))
(def thumb-left-wall-column 
  (let [thumb-column (case thumb-count :five 5/2 3/2)]
    (+ thumb-column 0.05)))
(def back-y (+ (first rows) #_0.02 -0.15))

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

(def front-wall
  (let [step wall-step ;;0.1
        wall-step 0.1 ;;0.05
        place case-place
        top-cover (fn [x-start x-end y-start y-end]
                    (top-case-cover place wall-sphere-top-front
                                    x-start x-end y-start y-end
                                    wall-step))]
    (union
     (apply union
            (for [x (range-inclusive 0.7 (- right-wall-column step) step)]
              (hull (place x cornerrow wall-sphere-top-front)
                    (place (+ x step) cornerrow wall-sphere-top-front)
                    (place x cornerrow wall-sphere-bottom-front)
                    (place (+ x step) cornerrow wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive 0.7 (- right-wall-column step) step)]
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
                     (key-place x cornerrow web-post-bl)
                     (key-place x cornerrow web-post-br))
               (hull (place (- x 1/2) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
                     (key-place x cornerrow web-post-bl)
                     (key-place (- x 1) cornerrow web-post-br)))))
     (hull (place right-wall-column cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (place (- right-wall-column 1) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (key-place penultcol cornerrow web-post-bl)
           (key-place penultcol cornerrow web-post-br))
     (hull (place (+ antecol 1/2) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (place (- right-wall-column 1) cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (key-place antecol cornerrow web-post-br)
           (key-place penultcol cornerrow web-post-bl))
     (hull (place 0.7 cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (place 1.7 cornerrow (translate [0 1 1] wall-sphere-bottom-front))
           (key-place 1 cornerrow web-post-bl)
           (key-place 1 cornerrow web-post-br)))))

(def back-wall
  (let [back-row (first rows)
        step wall-step
        wall-sphere-top-backtep 0.05
        place case-place
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
            (for [x (range-inclusive left-wall-column (- right-wall-column step) step)]
              (hull (place x back-y wall-sphere-top-back)
                    (place (+ x step) back-y wall-sphere-top-back)
                    (place x back-y wall-sphere-bottom-back)
                    (place (+ x step) back-y wall-sphere-bottom-back))))
     (apply union
            (for [x (range-inclusive left-wall-column (- right-wall-column step) step)]
              (bottom-hull (place x back-y wall-sphere-bottom-back)
                           (place (+ x step) back-y wall-sphere-bottom-back))))
     ;;     (front-top-cover left-wall-column 1.56 back-y (+ back-y 0.06))
     #_(front-top-cover left-wall-column right-wall-column back-y (+ back-y 0.06))
     ;;     (front-top-cover 1.56 2.44 back-y (+ back-y 0.06))
     (front-top-cover 3.56 4.44 back-y (+ back-y 0.2))
     (front-top-cover 4.3 right-wall-column back-y (+ back-y 0.2))


     (hull (place left-wall-column back-y (translate [1 -1 1] wall-sphere-bottom-back))
           (place (+ left-wall-column 1) back-y (translate [0 -1 1] wall-sphere-bottom-back))
           (key-place 0 back-row web-post-tl)
           (key-place 0 back-row web-post-tr))

     (hull (place penultcol back-y (translate [0 -1 1] wall-sphere-bottom-back))
           (place right-wall-column back-y (translate [0 -1 1] wall-sphere-bottom-back))
           (key-place penultcol back-row web-post-tl)
           (key-place penultcol back-row web-post-tr))

     (apply union
            (for [x (range 1 penultcol)]
              (union
               (hull (place (- x 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                     (place (+ x 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                     (key-place x back-row web-post-tl)
                     (key-place x back-row web-post-tr))
               (hull (place (- x 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                     (key-place x back-row web-post-tl)
                     (key-place (- x 1) back-row web-post-tr)))))
     (hull (place (- 4 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
           (place penultcol back-y (translate [0 -1 1] wall-sphere-bottom-back))
           (key-place antecol back-row web-post-tr)
           (key-place penultcol back-row web-post-tl)))))

(def right-wall
  (let [place case-place]
    (union
     (apply union
            (map (partial apply hull)
                 (partition 2 1
                            (for [scale (range-inclusive 0 1 0.01)]
                              (let [x (scale-to-range cornerrow back-y scale)]
                                (hull (place right-wall-column x (wall-sphere-top scale))
                                      (place right-wall-column x (wall-sphere-bottom scale))))))))

     (apply
      union
      (map (partial apply hull)
           (partition 2 1
                      (for [scale (range-inclusive 0 1 0.01)]
                        (let [x (scale-to-range cornerrow back-y scale)]
                          (bottom-hull (place right-wall-column x (wall-sphere-top scale))
                                       (place right-wall-column x (wall-sphere-bottom scale))))))))

     (apply union
            (concat
             (for [x (range 1 cornerrow)]
               (union
                (hull (place right-wall-column x (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                      (key-place penultcol x web-post-br)
                      (key-place penultcol x web-post-tr))))
             (for [x (range 1 cornerrow)]
               (union
                (hull (place right-wall-column x (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                      (place right-wall-column (inc x) (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                      (key-place penultcol x web-post-br)
                      (key-place penultcol (inc x) web-post-tr))))
             [(union
               (hull (place right-wall-column (first rows) (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                     (place right-wall-column back-y (translate [-1 -1 1] (wall-sphere-bottom 1)))
                     (key-place penultcol (first rows) web-post-tr))
               (hull (place right-wall-column cornerrow (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                     (place right-wall-column cornerrow (translate [-1 1 1] (wall-sphere-bottom 0)))
                     (key-place penultcol cornerrow web-post-br)))])))))

(def left-wall
  (let [place case-place]
    (union
     (apply union
            (for [x (range-inclusive (dec (first rows)) (- 1.6666 wall-step) wall-step)]
              (hull (place left-wall-column x wall-sphere-top-front)
                    (place left-wall-column (+ x wall-step) wall-sphere-top-front)
                    (place left-wall-column x wall-sphere-bottom-front)
                    (place left-wall-column (+ x wall-step) wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive (dec (first rows)) (- 1.6666 wall-step) wall-step)]
              (bottom-hull (place left-wall-column x wall-sphere-bottom-front)
                           (place left-wall-column (+ x wall-step) wall-sphere-bottom-front))))
     (hull (place left-wall-column (dec (first rows)) wall-sphere-top-front)
           (place left-wall-column (dec (first rows)) wall-sphere-bottom-front)
           (place left-wall-column back-y wall-sphere-top-back)
           (place left-wall-column back-y wall-sphere-bottom-back))

     (bottom-hull (place left-wall-column (dec (first rows)) wall-sphere-bottom-front)
                  (place left-wall-column back-y wall-sphere-bottom-back))
     #_(color [0 1 0] (hull (place left-wall-column 0 (translate [1 -1 1] wall-sphere-bottom-back))
                          (place left-wall-column 1 (translate [1 0 1] wall-sphere-bottom-back))
                          (key-place 0 0 web-post-tl)
                          (key-place 0 0 web-post-bl)))
     (color [0 1 0] (hull (place left-wall-column 1 (translate [1 -1 1] wall-sphere-bottom-back))
                          (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                          (key-place 0 1 web-post-tl)
                          (key-place 0 1 web-post-bl)))
     (color [1 0 0] (hull (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                          (place left-wall-column 1.6666  (translate [1 0 1] wall-sphere-bottom-front))
                          (key-place 0 1 web-post-bl)
                          (key-place 0 2 web-post-bl)))
     (color [1 1 0] (hull (place left-wall-column 1.6666  (translate [1 0 1] wall-sphere-bottom-front))
                          (key-place 0 2 web-post-bl)
                          (key-place 0 3 web-post-tl)))
     (hull (place left-wall-column 1.6666  (translate [1 0 1] wall-sphere-bottom-front))
           (thumb-place 1 1 web-post-tr)
           (key-place 0 3 web-post-tl))
     (hull (place left-wall-column 1.6666 (translate [1 0 1] wall-sphere-bottom-front))
           (thumb-place 1 1 web-post-tr)
           (thumb-place 1/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))))))

(def thumb-back-wall
  (let [step wall-step
        top-step 0.05
        front-top-cover (fn [x-start x-end y-start y-end]
                          (apply union
                                 (for [x (range-inclusive x-start (- x-end top-step) top-step)
                                       y (range-inclusive y-start (- y-end top-step) top-step)]
                                   (hull (thumb-place x y wall-sphere-top-back)
                                         (thumb-place (+ x top-step) y wall-sphere-top-back)
                                         (thumb-place x (+ y top-step) wall-sphere-top-back)
                                         (thumb-place (+ x top-step) (+ y top-step) wall-sphere-top-back)))))
        back-y thumb-back-y
        thumb-range (case thumb-count :five 5/2 3/2)]
    (union
     (apply union
            (for [x (range-inclusive 1/2 (- (+ thumb-range 0.05) step) step)]
              (hull (thumb-place x back-y wall-sphere-top-back)
                    (thumb-place (+ x step) back-y wall-sphere-top-back)
                    (thumb-place x back-y wall-sphere-bottom-back)
                    (thumb-place (+ x step) back-y wall-sphere-bottom-back))))
     (apply union
            (for [x (range-inclusive 1/2 (- (+ thumb-range 0.05) step) step)]
              (bottom-hull (thumb-place x back-y wall-sphere-bottom-back)
                           (thumb-place (+ x step) back-y wall-sphere-bottom-back))))
     (hull (thumb-place 1/2 back-y wall-sphere-top-back)
           (thumb-place 1/2 back-y wall-sphere-bottom-back)
           (case-place left-wall-column 1.6666 wall-sphere-top-front))
     (bottom-hull (thumb-place 1/2 back-y wall-sphere-bottom-back)
                  (case-place left-wall-column 1.7 wall-sphere-bottom-front))
     (hull (thumb-place 1/2 back-y wall-sphere-bottom-back)
           (case-place left-wall-column 1.6666 wall-sphere-top-front)
           (case-place left-wall-column 1.6666 wall-sphere-bottom-front))
     (hull
      (thumb-place 1/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
      (thumb-place 1 1 web-post-tr)
      (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
      (thumb-place 1 1 web-post-tl))
     (hull
      (thumb-place (+ 3/2 0.05) thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
      (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
      (thumb-place 1 1 web-post-tl)
      (thumb-place 1 1 web-post-tl)))))

(def thumb-left-wall
  (let [step wall-step
        place thumb-place
        column (case thumb-count :five 2 1)]
    (union
     (apply union
            (for [x (range-inclusive (+ -1 0.07) (- 1.95 step) step)]
              (hull (place thumb-left-wall-column x wall-sphere-top-front)
                    (place thumb-left-wall-column (+ x step) wall-sphere-top-front)
                    (place thumb-left-wall-column x wall-sphere-bottom-front)
                    (place thumb-left-wall-column (+ x step) wall-sphere-bottom-front))))
     (apply union
            (for [x (range-inclusive (+ -1 0.07) (- 1.95 step) step)]
              (bottom-hull (place thumb-left-wall-column x wall-sphere-bottom-front)
                           (place thumb-left-wall-column (+ x step) wall-sphere-bottom-front))))
     (hull (place thumb-left-wall-column 1.95 wall-sphere-top-front)
           (place thumb-left-wall-column 1.95 wall-sphere-bottom-front)
           (place thumb-left-wall-column thumb-back-y wall-sphere-top-back)
           (place thumb-left-wall-column thumb-back-y wall-sphere-bottom-back))

     (hull
      (thumb-place thumb-left-wall-column thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
      (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
      (thumb-place column 1 web-post-tl)
      (thumb-place column 1 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
      (thumb-place column 0 web-post-tl)
      (thumb-place column 1 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
      (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
      (thumb-place column 0 web-post-tl)
      (thumb-place column 0 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
      (thumb-place column -1 web-post-tl)
      (thumb-place column 0 web-post-bl))
     (hull
      (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
      (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [1 1 1] wall-sphere-bottom-front))
      (thumb-place column -1 web-post-tl)
      (thumb-place column -1 web-post-bl)))))

(def thumb-front-wall
  (let [step wall-step ;;0.1
        wall-sphere-top-fronttep 0.05 ;;0.05
        place thumb-place
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
           (case-place 0.5 cornerrow wall-sphere-top-front))
     (hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
                  (case-place 0.5 cornerrow wall-sphere-top-front))
     (bottom-hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
                  (case-place 0.7 cornerrow wall-sphere-bottom-front))
     (hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
           (case-place 0.5 cornerrow wall-sphere-top-front)
           (case-place 0.7 cornerrow wall-sphere-bottom-front))

     (hull (place thumb-right-wall thumb-front-row wall-sphere-bottom-front)
           (key-place 1 cornerrow web-post-bl)
           (place 0 -1/2 thumb-br)
           (place 0 -1/2 web-post-br)
           (case-place 0.7 cornerrow wall-sphere-bottom-front))

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

(def rj9-start [-35 35 0])
(def rj9-position [(first rj9-start) (second rj9-start) 11])
(def rj9-cube (cube 14.78 13 22.38))
(def rj9-space (translate rj9-position rj9-cube))
(def rj9-holder (translate rj9-position
                           (difference rj9-cube
                                       (union (translate [0 2 0] (cube 10.78  9 18.38))
                                              (translate [0 0 5] (cube 10.78 13  5))))))

(def usb-holder-position [-20 35 0])
(def usb-holder-size [6.5 10.0 13.6])
(def usb-holder-thickness 4)
(def usb-holder
  (->> (cube (+ (first usb-holder-size) usb-holder-thickness) (second usb-holder-size) (+ (last usb-holder-size) usb-holder-thickness))
       (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))
(def usb-holder-hole
  (->> (apply cube usb-holder-size)
       (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))

(def new-case
  (difference
   (union front-wall
          right-wall
          back-wall
          left-wall
          thumb-back-wall
          thumb-left-wall
          thumb-front-wall
          usb-holder)
   rj9-space))

;;;;;;;;;;;;
;; Bottom ;;
;;;;;;;;;;;;


(def bottom-key-guard (->> (cube mount-width mount-height web-thickness)
                           (translate [0 0 (+ (- (/ web-thickness 2)) -4.5)])))
(def bottom-front-key-guard (->> (cube mount-width (/ mount-height 2) web-thickness)
                                 (translate [0 (/ mount-height 4) (+ (- (/ web-thickness 2)) -4.5)])))

(def bottom-plate
  (union
   (apply union
          (for [column columns
                row (drop-last rows) ;;
                :when (or (not= column 0)
                          (not= row 4))]
            (->> bottom-key-guard
                 (key-place column row))))
   (thumb-layout-bottom (rotate (/ pi 2) [0 0 1] bottom-key-guard))
   (apply union
          (for [column columns
                row [(last rows)] ;;
                :when (or (not= column 0)
                          (not= row 4))]
            (->> bottom-front-key-guard
                 (key-place column row))))
   (let [shift #(translate [0 0 (+ (- web-thickness) -5)] %)
         web-post-tl (shift web-post-tl)
         web-post-tr (shift web-post-tr)
         web-post-br (shift web-post-br)
         web-post-bl (shift web-post-bl)
         half-shift-correction #(translate [0 (/ mount-height 2) 0] %)
         half-post-br (half-shift-correction web-post-br)
         half-post-bl (half-shift-correction web-post-bl)
         row-connections (concat
                          (for [column (drop-last columns)
                                row (drop-last rows)
                                :when (or (not= column 0)
                                          (not= row 4))]
                            (triangle-hulls
                             (key-place (inc column) row web-post-tl)
                             (key-place column row web-post-tr)
                             (key-place (inc column) row web-post-bl)
                             (key-place column row web-post-br)))
                          (for [column (drop-last columns)
                                row [(last rows)]
                                :when (or (not= column 0)
                                          (not= row 4))]
                            (triangle-hulls
                             (key-place (inc column) row web-post-tl)
                             (key-place column row web-post-tr)
                             (key-place (inc column) row half-post-bl)
                             (key-place column row half-post-br))))
         column-connections (for [column columns
                                  row (drop-last rows)
                                  :when (or (not= column 0)
                                            (not= row 3))]
                              (triangle-hulls
                               (key-place column row web-post-bl)
                               (key-place column row web-post-br)
                               (key-place column (inc row) web-post-tl)
                               (key-place column (inc row) web-post-tr)))
         diagonal-connections (for [column (drop-last columns)
                                    row (drop-last rows)
                                    :when (or (not= column 0)
                                              (not= row 3))]
                                (triangle-hulls
                                 (key-place column row web-post-br)
                                 (key-place column (inc row) web-post-tr)
                                 (key-place (inc column) row web-post-bl)
                                 (key-place (inc column) (inc row) web-post-tl)))
         main-keys-bottom (concat row-connections
                                  column-connections
                                  diagonal-connections)
         front-wall (concat
                     (for [x (range 2 5)]
                       (union
                        (hull (case-place (- x 1/2) 4 (translate [0 1 1] wall-sphere-bottom-front))
                              (case-place (+ x 1/2) 4 (translate [0 1 1] wall-sphere-bottom-front))
                              (key-place x 4 half-post-bl)
                              (key-place x 4 half-post-br))
                        (hull (case-place (- x 1/2) 4 (translate [0 1 1] wall-sphere-bottom-front))
                              (key-place x 4 half-post-bl)
                              (key-place (- x 1) 4 half-post-br))))
                     [(hull (case-place right-wall-column 4 (translate [0 1 1] wall-sphere-bottom-front))
                            (case-place (- right-wall-column 1) 4 (translate [0 1 1] wall-sphere-bottom-front))
                            (key-place 5 4 half-post-bl)
                            (key-place 5 4 half-post-br))
                      (hull (case-place (+ 4 1/2) 4 (translate [0 1 1] wall-sphere-bottom-front))
                            (case-place (- right-wall-column 1) 4 (translate [0 1 1] wall-sphere-bottom-front))
                            (key-place 4 4 half-post-br)
                            (key-place 5 4 half-post-bl))])
         right-wall (concat
                     (for [x (drop-last rows)]
                       (hull (case-place right-wall-column x (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                             (key-place 5 x web-post-br)
                             (key-place 5 x web-post-tr)))
                     (for [x (drop-last rows)]
                       (hull (case-place right-wall-column x (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                             (case-place right-wall-column (inc x) (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                             (key-place 5 x web-post-br)
                             (key-place 5 (inc x) web-post-tr)))
                     [(union
                       (hull (case-place right-wall-column (first rows) (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                             (case-place right-wall-column back-y (translate [-1 -1 1] (wall-sphere-bottom 1)))
                             (key-place 5 (first rows) web-post-tr)
                             )
                       (hull (case-place right-wall-column 4 (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                             (case-place right-wall-column 4 (translate [0 1 1] (wall-sphere-bottom 0)))
                             (key-place 5 4 half-post-br)
                             )
                       (hull (case-place right-wall-column 4 (translate [-1 0 1] (wall-sphere-bottom 1/2)))
                             (key-place 5 4 half-post-br)
                             (key-place 5 4 web-post-tr)))])
         back-wall (concat
                    (for [x (range 1 6)]
                      (union
                       (hull (case-place (- x 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                             (case-place (+ x 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                             (key-place x (first rows) web-post-tl)
                             (key-place x (first rows) web-post-tr))
                       (hull (case-place (- x 1/2) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                             (key-place x (first rows) web-post-tl)
                             (key-place (- x 1) (first rows) web-post-tr))))
                    [(hull (case-place left-wall-column back-y (translate [1 -1 1] wall-sphere-bottom-back))
                           (case-place (+ left-wall-column 1) back-y (translate [0 -1 1] wall-sphere-bottom-back))
                           (key-place 0 (first rows) web-post-tl)
                           (key-place 0 (first rows) web-post-tr))])
         left-wall (let [place case-place]
                     [(hull (place left-wall-column 1 (translate [1 -1 1] wall-sphere-bottom-back))
                            (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                            (key-place 0 1 web-post-tl)
                            (key-place 0 1 web-post-bl))
                      #_(hull (place left-wall-column 1 (translate [1 0 1] wall-sphere-bottom-back))
                              (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                              (key-place 0 0 web-post-bl)
                              (key-place 0 1 web-post-bl))
                      (hull (place left-wall-column 2 (translate [1 0 1] wall-sphere-bottom-back))
                            (place left-wall-column 1.6666  (translate [1 0 1] wall-sphere-bottom-front))
                            (key-place 0 1 web-post-bl)
                            (key-place 0 2 web-post-bl))
                      (hull (place left-wall-column 1.6666  (translate [1 0 1] wall-sphere-bottom-front))
                            (key-place 0 2 web-post-bl)
                            (key-place 0 3 web-post-tl))])

         #_(union
            (thumb-place 0 -1/2 shape)

            (thumb-place 1 7/8 shape)
            (thumb-place 1 -5/8 shape)

            (thumb-place 2 -3/4 shape)
            (thumb-place 2 3/4 shape)
            )


         thumb-tl #(->> web-post-tl
                        (translate [0 (extended-plate-height %) 0]))
         thumb-bl #(->> web-post-bl
                        (translate [0 (- (extended-plate-height %)) 0]))
         thumb-tr #(->> web-post-tr
                        (translate [0 (extended-plate-height %) 0]))
         thumb-br #(->> web-post-br
                        (translate [0 (- (extended-plate-height %)) 0]))

         thumbs [(triangle-hulls (thumb-place 0 -1/2 web-post-tl)
                                 (thumb-place 0 -1/2 web-post-bl)
                                 (thumb-place 1 -5/8 web-post-tr)
                                 (thumb-place 1 -5/8 web-post-br))
                 (hull (thumb-place 1 -5/8 web-post-tr)
                       (thumb-place 1 -5/8 web-post-tl)
                       (thumb-place 1 7/8 web-post-bl)
                       (thumb-place 1 7/8 web-post-br))
                 (hull (thumb-place 2 -3/4 web-post-tr)
                       (thumb-place 2 -3/4 web-post-tl)
                       (thumb-place 2 3/4 web-post-bl)
                       (thumb-place 2 3/4 web-post-br))
                 (triangle-hulls (thumb-place 2 3/4 web-post-tr)
                                 (thumb-place 1 7/8 web-post-tl)
                                 (thumb-place 2 3/4 web-post-br)
                                 (thumb-place 1 7/8 web-post-bl)
                                 (thumb-place 2 -3/4 web-post-tr)
                                 (thumb-place 1 -5/8 web-post-tl)
                                 (thumb-place 2 -3/4 web-post-br)
                                 (thumb-place 1 -5/8 web-post-bl)
                                 )]
         thumb-back-wall [(hull
                           (thumb-place 1/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
                           (thumb-place 1 7/8 web-post-tr)
                           (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
                           (thumb-place 1 7/8 web-post-tl))

                          (hull
                           (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
                           (thumb-place 3/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
                           (thumb-place 1 7/8 web-post-tl)
                           (thumb-place 2 3/4 web-post-tr))

                          (hull
                           (thumb-place (+ 5/2 0.05) thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
                           (thumb-place 2 3/4 web-post-tl)
                           (thumb-place 2 3/4 web-post-tr))

                          (hull
                           (thumb-place 1/2 thumb-back-y (translate [0 -1 1] wall-sphere-bottom-back))
                           (case-place left-wall-column 1.6666 (translate [1 0 1] wall-sphere-bottom-front))
                           (key-place 0 3 web-post-tl)
                           (thumb-place 1 7/8 web-post-tr))
                          ]
         thumb-left-wall [(hull
                           (thumb-place thumb-left-wall-column thumb-back-y (translate [1 -1 1] wall-sphere-bottom-back))
                           (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 1 web-post-tl)
                           (thumb-place 2 1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 0 web-post-tl)
                           (thumb-place 2 1 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column 0 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 0 web-post-tl)
                           (thumb-place 2 0 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place 2 -1 web-post-tl)
                           (thumb-place 2 0 web-post-bl))
                          (hull
                           (thumb-place thumb-left-wall-column -1 (translate [1 0 1] wall-sphere-bottom-back))
                           (thumb-place thumb-left-wall-column (+ -1 0.07) (translate [1 1 1] wall-sphere-bottom-front))
                           (thumb-place 2 -1 web-post-tl)
                           (thumb-place 2 -1 web-post-bl))]
         thumb-front-wall [(hull (thumb-place (+ 5/2 0.05) thumb-front-row (translate [1 1 1] wall-sphere-bottom-front))
                                 (thumb-place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place 2 -3/4 web-post-bl)
                                 (thumb-place 2 -3/4 web-post-br))

                           (hull (thumb-place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place (+ 3/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place 1 -5/8 web-post-bl)
                                 (thumb-place 1 -5/8 web-post-br)
                                 (thumb-place 2 -3/4 web-post-br))

                           (hull (thumb-place thumb-right-wall thumb-front-row (translate [-1 1 1] wall-sphere-bottom-front))
                                 (thumb-place (+ 1/2 0.05) thumb-front-row (translate [0 1 1] wall-sphere-bottom-front))
                                 (thumb-place 0 -1/2 web-post-bl)
                                 (thumb-place 1 -5/8 web-post-br)
                                 (thumb-place 0 -1/2 web-post-br))]
         thumb-inside [(triangle-hulls
                        (thumb-place 1 7/8 web-post-tr)
                        (key-place 0 3 web-post-tl)
                        (thumb-place 1 7/8 web-post-br)
                        (key-place 0 3 web-post-bl)
                        (thumb-place 1 -5/8 web-post-tr)
                        (thumb-place 0 -1/2 web-post-tl)
                        (key-place 0 3 web-post-bl)
                        (thumb-place 0 -1/2 web-post-tr)
                        (key-place 0 3 web-post-br)
                        (key-place 1 3 web-post-bl)
                        (thumb-place 0 -1/2 web-post-tr)
                        (key-place 1 4 web-post-tl)
                        (key-place 1 4 half-post-bl))

                       (hull
                        (thumb-place 0 -1/2 web-post-tr)
                        (thumb-place 0 -1/2 web-post-br)
                        (key-place 1 4 half-post-bl))

                       (hull
                        (key-place 1 4 half-post-bl)
                        (key-place 1 4 half-post-br)
                        (case-place (- 2 1/2) 4 (translate [0 1 1] wall-sphere-bottom-front))
                        (case-place 0.7 4 (translate [0 1 1] wall-sphere-bottom-front)))

                       (hull
                        (thumb-place 0 -1/2 web-post-br)
                        (thumb-place thumb-right-wall thumb-front-row (translate [-1 1 1] wall-sphere-bottom-front))
                        (key-place 1 4 half-post-bl)
                        )

                       (hull
                        (thumb-place thumb-right-wall thumb-front-row (translate [-1 1 1] wall-sphere-bottom-front))
                        (case-place 0.7 4 (translate [0 1 1] wall-sphere-bottom-front))
                        (thumb-place 0 -1/2 web-post-br)
                        (thumb-place 0 -1/2 web-post-tr)
                        )

                       #_                       (hull
                                                 (case-place 0.7 4 (translate [0 1 1] wall-sphere-bottom-front))
                                                 (thumb-place 0 -1/2 web-post-br)
                                                 (thumb-place 0 -1/2 web-post-tr)

                                                 )


                       #_                     (hull
                                               (thumb-place 0 -1 web-post-br)
                                               (thumb-place 0 -1/2 web-post-br)
                                               (thumb-place thumb-right-wall thumb-front-row (translate [-1 1 1] wall-sphere-bottom-front))
                                               (key-place 1 4 half-post-bl)
                                               )



                       ]
         stands (let [bumper-diameter 9.6
                      bumper-radius (/ bumper-diameter 2)
                      stand-diameter (+ bumper-diameter 2)
                      stand-radius (/ stand-diameter 2)
                      stand-at #(difference (->> (sphere stand-radius)
                                                 (translate [0 0 (+ (/ stand-radius -2) -4.5)])
                                                 %
                                                 (bottom-hull))
                                            (->> (cube stand-diameter stand-diameter stand-radius)
                                                 (translate [0 0 (/ stand-radius -2)])
                                                 %)
                                            (->> (sphere bumper-radius)
                                                 (translate [0 0 (+ (/ stand-radius -2) -4.5)])
                                                 %
                                                 (bottom 1.5)))]
                  [(stand-at #(key-place 0 1 %))
                   (stand-at #(thumb-place 1 -1/2 %))
                   (stand-at #(key-place 5 1 %))
                   (stand-at #(key-place 5 3 %))])]
     (apply union
            (concat
             main-keys-bottom
             front-wall
             right-wall
             back-wall
             left-wall
             thumbs
             thumb-back-wall
             thumb-left-wall
             thumb-front-wall
             thumb-inside
             stands)))))

(def screw-hole (->> (cylinder 1.5 60)
                     (translate [0 0 3/2])
                     (with-fn wall-sphere-n)))

(def screw-holes
  (union
   (key-place (+ 4 1/2) 3/2 screw-hole)
   (key-place (+ 4 1/2) (+ 3 1/2) screw-hole)
   (thumb-place 2 0 screw-hole)))

(defn circuit-cover [width length height]
  (let [cover-sphere-radius 1
        cover-sphere (->> (sphere cover-sphere-radius)
                          (with-fn 20))
        cover-sphere-z (+ (- height) (- cover-sphere-radius))
        cover-sphere-x (+ (/ width 2) cover-sphere-radius)
        cover-sphere-y (+ (/ length 2) (+ cover-sphere-radius))
        cover-sphere-tl (->> cover-sphere
                             (translate [(- cover-sphere-x) (- cover-sphere-y) cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-tr (->> cover-sphere
                             (translate [cover-sphere-x (- cover-sphere-y) cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-br (->> cover-sphere
                             (translate [cover-sphere-x cover-sphere-y cover-sphere-z])
                             (key-place 1/2 3/2))
        cover-sphere-bl (->> cover-sphere
                             (translate [(- cover-sphere-x) cover-sphere-y cover-sphere-z])
                             (key-place 1/2 3/2))

        lower-to-bottom #(translate [0 0 (+ (- cover-sphere-radius) -5.5)] %)
        bl (->> cover-sphere lower-to-bottom (key-place 0 1/2))
        br (->> cover-sphere lower-to-bottom (key-place 1 1/2))
        tl (->> cover-sphere lower-to-bottom (key-place 0 5/2))
        tr (->> cover-sphere lower-to-bottom (key-place 1 5/2))

        mlb (->> cover-sphere
                 (translate [(- cover-sphere-x) 0 (+ (- height) -1)])
                 (key-place 1/2 3/2))
        mrb (->> cover-sphere
                 (translate [cover-sphere-x 0 (+ (- height) -1)])
                 (key-place 1/2 3/2))

        mlt (->> cover-sphere
                 (translate [(+ (- cover-sphere-x) -4) 0 -6])
                 (key-place 1/2 3/2))
        mrt (->> cover-sphere
                 (translate [(+ cover-sphere-x 4) 0 -6])
                 (key-place 1/2 3/2))]
    (union
     (hull cover-sphere-bl cover-sphere-br cover-sphere-tl cover-sphere-tr)
     (hull cover-sphere-br cover-sphere-bl bl br)
     (hull cover-sphere-tr cover-sphere-tl tl tr)
     (hull cover-sphere-tl tl mlb mlt)
     (hull cover-sphere-bl bl mlb mlt)
     (hull cover-sphere-tr tr mrb mrt)
     (hull cover-sphere-br br mrb mrt))))

(def io-exp-width 10)
(def io-exp-height 8)
(def io-exp-length 36)

(def teensy-width 20)
(def teensy-height 12)
(def teensy-length 33)

(def io-exp-cover (circuit-cover io-exp-width io-exp-length io-exp-height))
(def teensy-cover (circuit-cover teensy-width teensy-length teensy-height))

(def trrs-diameter 6.6)
(def trrs-radius (/ trrs-diameter 2))
(def trrs-hole-depth 10)

(def trrs-hole (->> (union (cylinder trrs-radius trrs-hole-depth)
                           (->> (cube trrs-diameter (+ trrs-radius 5) trrs-hole-depth)
                                (translate [0 (/ (+ trrs-radius 5) 2) 0])))
                    (rotate (/ pi 2) [1 0 0])
                    (translate [0 (+ (/ mount-height 2) 4) (- trrs-radius)])
                    (with-fn 50)))

(def trrs-hole-just-circle
  (->> (cylinder trrs-radius trrs-hole-depth)
       (rotate (/ pi 2) [1 0 0])
       (translate [0 (+ (/ mount-height 2) 4) (- trrs-radius)])
       (with-fn 50)
       (key-place 1/2 0)))

(def trrs-box-hole (->> (cube 14 14 7 )
                        (translate [0 1 -3.5])))


(def trrs-cutout
  (->> (union trrs-hole
              trrs-box-hole)
       (key-place 1/2 1)))

(def teensy-pcb-thickness 1.6)
(def teensy-offset-height 5)

(def teensy-pcb (->> (cube 18 30.5 teensy-pcb-thickness)
                     (translate [0 0 (+ (/ teensy-pcb-thickness -2) (- teensy-offset-height))])
                     (key-place 1/2 3/2)
                     (color [1 0 0])))

(def teensy-support
  (difference
   (union
    (->> (cube 3 3 9)
         (translate [0 0 -2])
         (key-place 1/2 3/2)
         (color [0 1 0]))
    (hull (->> (cube 3 6 9)
               (translate [0 0 -2])
               (key-place 1/2 2)
               (color [0 0 1]))
          (->> (cube 3 3 (+ teensy-pcb-thickness 3))
               (translate [0 (/ 30.5 -2) (+ (- teensy-offset-height)
                                            #_(/ (+ teensy-pcb-thickness 3) -2)
                                            )])
               (key-place 1/2 3/2)
               (color [0 0 1]))))
   teensy-pcb
   (->> (cube 18 30.5 teensy-pcb-thickness)
        (translate [0 1.5 (+ (/ teensy-pcb-thickness -2) (- teensy-offset-height) -1)])
        (key-place 1/2 3/2)
        (color [1 0 0]))))

(def usb-cutout
  (let [hole-height 6.2
        side-radius (/ hole-height 2)
        hole-width 10.75
        side-cylinder (->> (cylinder side-radius teensy-length)
                           (with-fn 20)
                           (translate [(/ (- hole-width hole-height) 2) 0 0]))]
    (->> (hull side-cylinder
               (mirror [-1 0 0] side-cylinder))
         (rotate (/ pi 2) [1 0 0])
         (translate [0 (/ teensy-length 2) (- side-radius)])
         (translate [0 0 (- 1)])
         (translate [0 0 (- teensy-offset-height)])
         (key-place 1/2 3/2))))

;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;

(def dactyl-top-right
  (difference
   (union key-holes
          connectors
          thumb
          new-case
          rj9-holder
          #_teensy-support
          #_caps
          #_thumbcaps)
   usb-holder-hole
   #_trrs-hole-just-circle
   #_screw-holes))

#_(def dactyl-bottom-right
  bottom-plate
  #_(difference
     (union
      teensy-cover
      (difference
       bottom-plate
       (hull teensy-cover)
       new-case
       teensy-cover
       trrs-cutout
       (->> (cube 1000 1000 10) (translate [0 0 -5]))
       screw-holes))
     usb-cutout
     dactyl-top-right))

#_(def dactyl-bottom-left
  (mirror [-1 0 0]
          (union
           io-exp-cover
           (difference
            bottom-plate
            (hull io-exp-cover)
            new-case
            io-exp-cover
            trrs-cutout
            (->> (cube 1000 1000 10) (translate [0 0 -5]))
            screw-holes))))

#_(def dactyl-top-left
  (mirror [-1 0 0]
          (difference
           (union key-holes
                  connectors
                  thumb
                  new-case)
           trrs-hole-just-circle
           screw-holes)))

(spit "things/lightcycle-cherry-top-right.scad"
      (write-scad dactyl-top-right))

(comment
  (spit "things/lightcycle-cherry-bottom-right.scad"
        (write-scad dactyl-bottom-right))

  (spit "things/lightcycle-cherry-top-left.scad"
        (write-scad dactyl-top-left))

  (spit "things/lightcycle-cherry-bottom-left.scad"
        (write-scad dactyl-bottom-left)))

(comment
  (spit "things/lightcycle-matias-top-right.scad"
        (write-scad dactyl-top-right))

  (spit "things/lightcycle-matias-bottom-right.scad"
        (write-scad dactyl-bottom-right))

  (spit "things/lightcycle-matias-top-left.scad"
        (write-scad dactyl-top-left))

  (spit "things/lightcycle-matias-bottom-left.scad"
        (write-scad dactyl-bottom-left)))