(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

; number of rows.
; 4 means your pinky and index fingers will get three rows.
(def nrows 4)

; number of columns.
; 5 means your left hand will get a through g.
(def ncols 5)

; curvature of the columns
(def α (/ π 12))
; curvature of the rows
(def β (/ π 36))
; controls front-back tilt
(def centerrow (- nrows 3))
; controls left-right tilt / tenting (higher number is more tenting)
(def centercol 4)
; or, change this for more precise tenting control
(def tenting-angle (/ π 9))  
; options include :standard, :orthographic, and :fixed
(def column-style :standard) 

; if you don't want the side nubs, set this
; parameter as false.
; otherwise, set it as true.
(def create-side-nub? false)

; if you want to have wire posts, set this
; parameter as true.
(def use-wire-post? false)

; if you want to use trrs instead of rj9, set
; this parameter as true
(def use-trrs? false)

; if you want to create a "rental car", set this parameter as true
(def rental-car? false)

; if you want to use small usb hole, set
; this parameter as true
(def use-promicro-usb-hole? false)

; wide pinky, 1.5u.
(def use-wide-pinky? false)

; inner column like a weird dude.
(def use-inner-column? false)

; should be used with `use-wide-pinky` and
; `use-inner-column` for ergodox like thingy.
(def use-last-rows? false)

; show caps on the right.scad file.
; set it true if you want to see the result.
; set it false when you want to actually print it.
(def show-caps? false)

; if this param set as true, you will only get 3 thumb keys. 3 1.5us.
; otherwise, you will get standard thumb cluster.
(def minidox-style? true)

; if this param set as true, you will have a hotswap holder.
(def use-hotswap? false)

; when you set `rental-car?` as true, you will get a lined up
; or un-staggered columns in x and y axis.
(defn column-offset [column]
  (if rental-car?
    (cond (= column 2)  [0   0    -6.5]
          (>= column 4) [0   0     6]
          :else         [0   0     0])
    (cond (= column 2)  [0   2.82 -6.5]
          (>= column 4) [0  -13    6]
          :else         [0   0     0])))

; it dictates the location of the thumb cluster.
; the first member of the vector is x axis, second one y axis,
; while the last one is y axis.
; the higher x axis value is, the closer it to the pinky.
; the higher y axis value is, the closer it to the alphas.
; the higher z axis value is, the higher it is.
(def thumb-offsets [6 -3 7])

; controls overall height; original=9 with centercol=3; use 16 for centercol=2
(def keyboard-z-offset 4)

; extra space between the base of keys; original= 2
(def extra-width 2.5)
; original= 0.5
(def extra-height 1.0)

; length of the first downward-sloping part of the wall (negative)
(def wall-z-offset -15)
; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)
; wall thickness parameter; originally 5
(def wall-thickness 2)

;; Settings for column-style == :fixed 
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def middlerow (dec cornerrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

; Was 14.1, then 14.25
(def keyswitch-height 14.0)
(def keyswitch-width 14.0)

(def sa-profile-key-height 12.7)

(def plate-thickness 5)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

; each and every single switch hole is defined by this function.
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
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 plate-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ plate-thickness 2)]))))
        ; the hole's wall.
        plate-half (union top-wall
                          left-wall
                          (if create-side-nub? (with-fn 100 side-nub) ()))
        ; the bottom of the hole.
        swap-holder (->> (cube (+ keyswitch-width 3) (/ (+ keyswitch-height 3) 2) 3)
                         (translate [0 (/ (+ keyswitch-height 3) 4) -1.5]))
        ; for the main axis
        main-axis-hole (->> (cylinder (/ 4.0 2) 10)
                            (with-fn 12))
        plus-hole (->> (cylinder (/ 3.3 2) 10)
                       (with-fn 8)
                       (translate [-3.81 2.54 0]))
        minus-hole (->> (cylinder (/ 3.3 2) 10)
                        (with-fn 8)
                        (translate [2.54 5.08 0]))
        plus-hole-mirrored (->> (cylinder (/ 3.3 2) 10)
                                (with-fn 8)
                                (translate [3.81 2.54 0]))
        minus-hole-mirrored (->> (cylinder (/ 3.3 2) 10)
                                 (with-fn 8)
                                 (translate [-2.54 5.08 0]))
        friction-hole (->> (cylinder (/ 1.7 2) 10)
                           (with-fn 8))
        friction-hole-right (translate [5 0 0] friction-hole)
        friction-hole-left (translate [-5 0 0] friction-hole)
        hotswap-base-shape (->> (cube 19 6.2 3.5)
                                (translate [0 4 -2.6]))
        hotswap-holder (difference swap-holder
                                   main-axis-hole
                                   plus-hole
                                   minus-hole
                                   plus-hole-mirrored
                                   minus-hole-mirrored
                                   friction-hole-left
                                   friction-hole-right
                                   hotswap-base-shape)]
    (difference (union plate-half
                       (->> plate-half
                            (mirror [1 0 0])
                            (mirror [0 1 0]))
                       (if use-hotswap? hotswap-holder ())))))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap
  {1 (let [bl2 (/ 18.5 2)
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

; an array of columns from 0 to number of columns.
(def columns (range 0 ncols))
(def inner-columns (range -1 ncols))
(def rows (range 0 nrows))
(def inner-rows (range 0 cornerrow))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))
(def column-base-angle (* β (- centercol 2)))

; when set `use-wide-pinky?`,
; you will get 1.5u keys for the outermost pinky keys.
(defn offset-for-column [col row]
  (if (and use-wide-pinky?
           (not= row lastrow)
           (= col lastcol))
    5.5
    0))

; this is the helper function to 'place' the keys on the defined curve
; of the board.
(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column row) 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))]
    (->> placed-shape
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

; this is the function that puts the key switch holes
; based on the row and the column.
(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (if use-last-rows?
                       (or (not (.contains [0 1] column)) (not= row lastrow))
                       (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(defn key-inner-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(def inner-key-holes
  (apply union (for [row inner-rows]
                 (->> single-plate
                      (key-inner-place -1 row)))))

(def caps
  (apply
   union
   (for [column (if use-inner-column? (range -1 ncols) columns)
         row rows
         :when (if use-last-rows?
                 (or (not (.contains [-1 0 1] column)) (not= row lastrow))
                 (not= row lastrow))
         :when (if use-inner-column?
                 (not (and (.contains [-1] column)
                           (or (= row cornerrow)
                               (= row lastrow))))
                 true)]
     (->> (sa-cap (if (and use-wide-pinky?
                           (= column lastcol)
                           (not= row lastrow))
                    1.5
                    1))
          (key-place column row)))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def web-post
  (->> (cube post-size post-size web-thickness)
       (translate [0 0 (+ (/ web-thickness -2)
                          plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

(def wide-post-tr
  (if use-wide-pinky?
    (translate [(- (/ mount-width  1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post)
    web-post-tr))
(def wide-post-tl
  (if use-wide-pinky?
    (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post)
    web-post-tl))
(def wide-post-bl
  (if use-wide-pinky?
    (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post)
    web-post-bl))
(def wide-post-br
  (if use-wide-pinky?
    (translate [(- (/ mount-width  1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post)
    web-post-br))

;; takes a list of 'location's,
;; partitions them into triad,
;; and apply hull on each triad,
;; then finally apply union for the result.
(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply
   union
   (concat
          ;; Row connections
    (for [column (range (if use-inner-column? -1 0) (dec ncols))
          row (range 0 (if use-last-rows? (inc lastrow) lastrow))
          :when (if use-last-rows?
                  (not (and (= row lastrow)
                            (.contains [-1 0 1] column)))
                  true)]
      (triangle-hulls
       (key-place (inc column) row web-post-tl)
       (key-place column row web-post-tr)
       (key-place (inc column) row web-post-bl)
       (if (not (and (= column -1)
                     (= row cornerrow)))
         (key-place column row web-post-br)
         ())))

          ;; Column connections
    (for [column (if use-inner-column? inner-columns columns)
          row (range 0 (if use-last-rows? lastrow cornerrow))
          :when (if use-last-rows?
                  (not (and (= row cornerrow)
                            (.contains [-1 0 1] column)))
                  true)]
      (triangle-hulls
       (key-place column row web-post-br)
       (key-place column row web-post-bl)
       (key-place column (inc row) web-post-tr)
       (if (not (and (= column -1)
                     (= row middlerow)))
         (key-place column (inc row) web-post-tl)
         ())))

          ;; Diagonal connections
    (for [column (range (if use-inner-column? -1 0) (dec ncols))
          row (range 0 (if use-last-rows? lastrow cornerrow))
          :when (if use-last-rows?
                  (not (and (= row lastrow)
                            (.contains [-1 0 1] column)))
                  true)]
      (triangle-hulls
       (key-place column row web-post-br)
       (key-place column (inc row) web-post-tr)
       (key-place (inc column) row web-post-bl)
       (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

; this is where the original position of the thumb switches defined.
; each and every thumb keys is derived from this value.
; the value itself is defined from the 'm' key's position in qwerty layout
; and then added by some values, including thumb-offsets above.
(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(defn thumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-12 -16 3])))
(defn thumb-tl-place [shape]
  (let [movement (if minidox-style? [-35 -15 -2] [-32 -15 -2])
        z-rotation (if minidox-style? 20 10)]
    (->> shape
         (rotate (deg2rad  10) [1 0 0])
         (rotate (deg2rad -23) [0 1 0])
         (rotate (deg2rad  z-rotation) [0 0 1])
         (translate thumborigin)
         (translate movement))))
(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -6) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  48) [0 0 1])
       (translate thumborigin)
       (translate [-29 -40 -13])))
(defn thumb-ml-place [shape]
  (let [movement (if minidox-style? [-53 -26 -12] [-51 -25 -12])]
    (->> shape
         (rotate (deg2rad   6) [1 0 0])
         (rotate (deg2rad -34) [0 1 0])
         (rotate (deg2rad  40) [0 0 1])
         (translate thumborigin)
         (translate movement))))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad -16) [1 0 0])
       (rotate (deg2rad -33) [0 1 0])
       (rotate (deg2rad  54) [0 0 1])
       (translate thumborigin)
       (translate [-37.8 -55.3 -25.3])))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad  -4) [1 0 0])
       (rotate (deg2rad -35) [0 1 0])
       (rotate (deg2rad  52) [0 0 1])
       (translate thumborigin)
       (translate [-56.3 -43.3 -23.5])))

(defn thumb-1x-layout [shape]
  (union
   (if-not minidox-style?
     (union
      (thumb-ml-place shape)
      (thumb-mr-place shape)
      (thumb-bl-place shape)
      (thumb-br-place shape)))))

(defn thumb-15x-layout [shape]
  (union
   (if minidox-style? (thumb-ml-place shape)())
   (thumb-tr-place shape)
   (thumb-tl-place shape)))

(def larger-plate
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)
   (thumb-15x-layout larger-plate)))

(def thumb-post-tr
  (translate [(- (/ mount-width 2) post-adj)
              (- (/ mount-height  1.15) post-adj)
              0] web-post))
(def thumb-post-tl
  (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-bl
  (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.15) post-adj) 0] web-post))
(def thumb-post-br
  (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.15) post-adj) 0] web-post))

(def thumb-connectors
  (if minidox-style?
    (union
     (triangle-hulls    ; top two
      (thumb-tl-place thumb-post-tr)
      (thumb-tl-place thumb-post-br)
      (thumb-tr-place thumb-post-tl)
      (thumb-tr-place thumb-post-bl)
      (thumb-tl-place thumb-post-br)
      (thumb-tl-place thumb-post-bl))
     (triangle-hulls    ; top two to the middle two, starting on the left
      (thumb-tl-place thumb-post-tl)
      (thumb-ml-place thumb-post-tr)
      (thumb-tl-place thumb-post-bl)
      (thumb-ml-place thumb-post-br))
     (triangle-hulls    ; top two to the main keyboard, starting on the left
      (thumb-tl-place thumb-post-tl)
      (key-place 0 cornerrow web-post-bl)
      (thumb-tl-place thumb-post-tr)
      (key-place 0 cornerrow web-post-br)
      (thumb-tr-place thumb-post-tl)
      (key-place 1 cornerrow web-post-bl)
      (thumb-tr-place thumb-post-tr)
      (key-place 1 cornerrow web-post-br)
      (thumb-tr-place thumb-post-br)
      (key-place 2 cornerrow web-post-bl)
      (if use-last-rows?
        (key-place 2 lastrow web-post-bl)
        ())
      (key-place 2 (if use-last-rows? lastrow cornerrow) web-post-bl)
      (key-place 2 (if use-last-rows? lastrow cornerrow) web-post-br)
      (thumb-tr-place thumb-post-br)
      (key-place 3 (if use-last-rows? lastrow cornerrow) web-post-bl))
     (triangle-hulls
      (thumb-tl-place thumb-post-bl)
      (thumb-ml-place thumb-post-br)
      (thumb-ml-place thumb-post-bl))
     (triangle-hulls
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 2 cornerrow web-post-br)
      (key-place 3 cornerrow web-post-bl))
     (triangle-hulls
      (key-place 3 lastrow web-post-tr)
      (key-place 4 cornerrow web-post-bl)))
    (union
     (triangle-hulls    ; top two
      (thumb-tl-place thumb-post-tr)
      (thumb-tl-place thumb-post-br)
      (thumb-tr-place thumb-post-tl)
      (thumb-tr-place thumb-post-bl))
     (triangle-hulls    ; bottom two on the right
      (thumb-br-place web-post-tr)
      (thumb-br-place web-post-br)
      (thumb-mr-place web-post-tl)
      (thumb-mr-place web-post-bl))
     (triangle-hulls    ; bottom two on the left
      (thumb-bl-place web-post-tr)
      (thumb-bl-place web-post-br)
      (thumb-ml-place web-post-tl)
      (thumb-ml-place web-post-bl))
     (triangle-hulls    ; centers of the bottom four
      (thumb-br-place web-post-tl)
      (thumb-bl-place web-post-bl)
      (thumb-br-place web-post-tr)
      (thumb-bl-place web-post-br)
      (thumb-mr-place web-post-tl)
      (thumb-ml-place web-post-bl)
      (thumb-mr-place web-post-tr)
      (thumb-ml-place web-post-br))
     (triangle-hulls    ; top two to the middle two, starting on the left
      (thumb-tl-place thumb-post-tl)
      (thumb-ml-place web-post-tr)
      (thumb-tl-place thumb-post-bl)
      (thumb-ml-place web-post-br)
      (thumb-tl-place thumb-post-br)
      (thumb-mr-place web-post-tr)
      (thumb-tr-place thumb-post-bl)
      (thumb-mr-place web-post-br)
      (thumb-tr-place thumb-post-br))
     (triangle-hulls    ; top two to the main keyboard, starting on the left
      (thumb-tl-place thumb-post-tl)
      (key-place 0 cornerrow web-post-bl)
      (thumb-tl-place thumb-post-tr)
      (key-place 0 cornerrow web-post-br)
      (thumb-tr-place thumb-post-tl)
      (key-place 1 cornerrow web-post-bl)
      (thumb-tr-place thumb-post-tr)
      (key-place 1 cornerrow web-post-br)
      (thumb-tr-place thumb-post-br)
      (key-place 2 cornerrow web-post-bl)
      (if use-last-rows?
        (key-place 2 lastrow web-post-bl)
        ())
      (key-place 2 (if use-last-rows? lastrow cornerrow) web-post-bl)
      (key-place 2 (if use-last-rows? lastrow cornerrow) web-post-br)
      (thumb-tr-place thumb-post-br)
      (key-place 3 (if use-last-rows? lastrow cornerrow) web-post-bl))
     (triangle-hulls
      (key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 2 cornerrow web-post-br)
      (key-place 3 cornerrow web-post-bl)))))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 10)
(def left-wall-z-offset  3)

(defn left-key-position [row direction]
  (map -
       (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0])
       [left-wall-x-offset 0 left-wall-z-offset]))

(defn inner-key-position [row direction]
  (map -
       (key-position -1 row [(* mount-width -0.5) (* direction mount-height 0.5) 0])
       [left-wall-x-offset 0 left-wall-z-offset]))

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn inner-key-place [row direction shape]
  (translate (inner-key-position row direction) shape))

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

(def right-wall
  (union (key-wall-brace lastcol 0 0 1 wide-post-tr lastcol 0 1 0 wide-post-tr)
         (for [y (range 0 lastrow)]
           (key-wall-brace lastcol y 1 0 wide-post-tr lastcol y 1 0 wide-post-br))
         (if use-last-rows?
           (key-wall-brace lastcol lastrow 1 0 wide-post-tr lastcol lastrow 1 0 wide-post-br)
           ())
         (for [y (range 1 lastrow)]
           (key-wall-brace lastcol (dec y) 1 0 wide-post-br lastcol y 1 0 wide-post-tr))
         (if use-last-rows?
           (key-wall-brace lastcol (dec lastrow) 1 0 wide-post-br lastcol lastrow 1 0 wide-post-tr)
           ())
         (key-wall-brace lastcol (if use-last-rows? lastrow cornerrow) 0 -1 wide-post-br
                         lastcol (if use-last-rows? lastrow cornerrow) 1 0 wide-post-br)))

(def pinky-connectors
  (apply union
         (concat
          (for [row (range 0 (if use-last-rows? (inc lastrow) lastrow))]
            (triangle-hulls
             (key-place lastcol row web-post-tr)
             (key-place lastcol row wide-post-tr)
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)))
          (for [row (range 0 (if use-last-rows? lastrow cornerrow))]
            (triangle-hulls
             (key-place lastcol row web-post-br)
             (key-place lastcol row wide-post-br)
             (key-place lastcol (inc row) web-post-tr)
             (key-place lastcol (inc row) wide-post-tr))))))

(def pinky-walls
  (union
   (key-wall-brace lastcol (if use-last-rows? lastrow cornerrow) 0 -1 web-post-br
                   lastcol (if use-last-rows? lastrow cornerrow) 0 -1 wide-post-br)
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)))

(def case-walls
  (union
   ; back wall
   (for [x (range (if use-inner-column? -1 0) ncols)]
     (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
   (for [x (range (if use-inner-column?  0 1) ncols)]
     (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   ; right wall
   right-wall
   ; left wall
   (for [y (range 0 (if use-inner-column? cornerrow lastrow))]
     (union
      (wall-brace (partial (if use-inner-column? inner-key-place left-key-place) y  1) -1 0 web-post
                  (partial (if use-inner-column? inner-key-place left-key-place) y -1) -1 0 web-post)
      (hull (key-place (if use-inner-column? -1 0) y web-post-tl)
            (key-place (if use-inner-column? -1 0) y web-post-bl)
            ((if use-inner-column? inner-key-place left-key-place) y  1 web-post)
            ((if use-inner-column? inner-key-place left-key-place) y -1 web-post))))
   (for [y (range 1 (if use-inner-column? cornerrow lastrow))]
     (union
      (wall-brace (partial (if use-inner-column? inner-key-place left-key-place) (dec y) -1) -1 0 web-post
                  (partial (if use-inner-column? inner-key-place left-key-place) y        1) -1 0 web-post)
      (hull (key-place (if use-inner-column? -1 0) y       web-post-tl)
            (key-place (if use-inner-column? -1 0) (dec y) web-post-bl)
            ((if use-inner-column? inner-key-place left-key-place) y        1 web-post)
            ((if use-inner-column? inner-key-place left-key-place) (dec y) -1 web-post))))
   (wall-brace (partial key-place (if use-inner-column? -1 0) 0) 0 1 web-post-tl
               (partial (if use-inner-column? inner-key-place left-key-place) 0 1)  0 1 web-post)
   (wall-brace (partial (if use-inner-column? inner-key-place left-key-place) 0 1)  0 1 web-post
               (partial (if use-inner-column? inner-key-place left-key-place) 0 1) -1 0 web-post)
   ; front wall
   (key-wall-brace 3 (if use-last-rows? lastrow cornerrow) 0   -1 web-post-bl
                   3 (if use-last-rows? lastrow cornerrow) 0.5 -1 web-post-br)
   (key-wall-brace 3 (if use-last-rows? lastrow cornerrow) 0.5 -1 web-post-br
                   4 (if use-last-rows? lastrow cornerrow) 0   -1 web-post-bl)
   (for [x (range 4 ncols)]
     (key-wall-brace x (if use-last-rows? lastrow cornerrow) 0 -1 web-post-bl
                     x (if use-last-rows? lastrow cornerrow) 0 -1 web-post-br))
   (for [x (range 5 ncols)]
     (key-wall-brace x       (if use-last-rows? lastrow cornerrow) 0 -1 web-post-bl
                     (dec x) (if use-last-rows? lastrow cornerrow) 0 -1 web-post-br))
   ; thumb walls
   (if minidox-style?
     (union
      (wall-brace thumb-ml-place 0  1 thumb-post-tr thumb-ml-place  0  1 thumb-post-tl)
      (wall-brace thumb-tr-place 0 -1 thumb-post-br thumb-tr-place  0 -2 thumb-post-bl)
      (wall-brace thumb-tr-place 0 -2 thumb-post-bl thumb-tl-place  0 -2 thumb-post-bl)
      (wall-brace thumb-tl-place 0 -2 thumb-post-bl thumb-ml-place -1 -1 thumb-post-bl))
     (union
      (wall-brace thumb-mr-place  0   -1 web-post-br   thumb-tr-place  0 -1 thumb-post-br)
      (wall-brace thumb-mr-place  0   -1 web-post-br   thumb-mr-place  0 -1 web-post-bl)
      (wall-brace thumb-br-place  0   -1 web-post-br   thumb-br-place  0 -1 web-post-bl)
      (wall-brace thumb-ml-place -0.3  1 web-post-tr   thumb-ml-place  0  1 web-post-tl)
      (wall-brace thumb-bl-place  0    1 web-post-tr   thumb-bl-place  0  1 web-post-tl)
      (wall-brace thumb-br-place -1    0 web-post-tl   thumb-br-place -1  0 web-post-bl)
      (wall-brace thumb-bl-place -1    0 web-post-tl   thumb-bl-place -1  0 web-post-bl)))
   ; thumb corners
   (if minidox-style?
     (union (wall-brace thumb-ml-place -1  0 thumb-post-tl thumb-ml-place -1  0 thumb-post-bl)
            (wall-brace thumb-ml-place -1  0 thumb-post-bl thumb-ml-place -1 -1 thumb-post-bl)
            (wall-brace thumb-ml-place -1  0 thumb-post-tl thumb-ml-place  0  1 thumb-post-tl))
     (union (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
            (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)))
   ; thumb tweeners
   (wall-brace thumb-tr-place  0 -1 thumb-post-br
               (partial key-place 3 (if use-last-rows? lastrow cornerrow))  0 -1 web-post-bl)
   (if-not minidox-style?
     (union
      (wall-brace thumb-mr-place  0 -1 web-post-bl   thumb-br-place  0 -1 web-post-br)
      (wall-brace thumb-ml-place  0  1 web-post-tl   thumb-bl-place  0  1 web-post-tr)
      (wall-brace thumb-bl-place -1  0 web-post-bl   thumb-br-place -1  0 web-post-tl)))
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
    (if use-inner-column?
      (inner-key-place middlerow -1 (translate (wall-locate2 -1 0) web-post))
      (left-key-place  cornerrow -1 (translate (wall-locate2 -1 0) web-post)))
    (if use-inner-column?
      (inner-key-place middlerow -1 (translate (wall-locate3 -1 0) web-post))
      (left-key-place  cornerrow -1 (translate (wall-locate3 -1 0) web-post)))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr)))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr))))
   (hull
    (if use-inner-column?
      (inner-key-place middlerow -1 (translate (wall-locate2 -1 0) web-post))
      (left-key-place  cornerrow -1 (translate (wall-locate2 -1 0) web-post)))
    (if use-inner-column?
      (inner-key-place middlerow -1 (translate (wall-locate3 -1 0) web-post))
      (left-key-place  cornerrow -1 (translate (wall-locate3 -1 0) web-post)))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr)))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr)))
    (thumb-tl-place thumb-post-tl))
   (if use-inner-column?
     (hull
      (inner-key-place middlerow -1 web-post)
      (inner-key-place middlerow -1 (translate (wall-locate1 -1 0) web-post))
      (inner-key-place middlerow -1 (translate (wall-locate2 -1 0) web-post))
      (inner-key-place middlerow -1 (translate (wall-locate3 -1 0) web-post))
      (thumb-tl-place thumb-post-tl))
     (hull
      (left-key-place cornerrow -1 web-post)
      (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
      (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
      (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
      (thumb-tl-place thumb-post-tl)))
   (if use-inner-column?
     (hull
      (inner-key-place middlerow -1 web-post)
      (inner-key-place middlerow -1 (translate (wall-locate1 -1 0) web-post))
      (key-place -1 middlerow web-post-bl)
      (key-place -1 middlerow (translate (wall-locate1 -1 0) web-post-bl))
      (thumb-tl-place thumb-post-tl))
     (hull
      (left-key-place cornerrow -1 web-post)
      (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
      (key-place 0 cornerrow web-post-bl)
      (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
      (thumb-tl-place thumb-post-tl)))
   (if use-inner-column?
     (triangle-hulls
      (thumb-tl-place thumb-post-tl)
      (key-place  0 cornerrow web-post-bl)
      (key-place -1 middlerow web-post-bl)
      (key-place -1 cornerrow web-post-tr))
     ())
   (hull
    (thumb-ml-place (if minidox-style? thumb-post-tr web-post-tr))
    (thumb-ml-place (translate (wall-locate1 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr)))
    (thumb-ml-place (translate (wall-locate2 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr)))
    (thumb-ml-place (translate (wall-locate3 -0.3 1) (if minidox-style? thumb-post-tr web-post-tr)))
    (thumb-tl-place thumb-post-tl))))

(def rj9-start
  (map + [0 -3  0] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def rj9-position
  [(first rj9-start) (second rj9-start) 11])
(def rj9-cube
  (cube 14.78 13 22.38))
(def rj9-space
  (translate rj9-position rj9-cube))
(def rj9-holder
  (translate
   rj9-position
   (difference rj9-cube
               (union (translate [0 2 0] (cube 10.78  9 18.38))
                      (translate [0 0 5] (cube 10.78 13  5))))))

(def usb-holder-position (key-position 1 0 (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0])))
(def usb-holder-size [6.5 10.0 13.6])
(def usb-holder-thickness 4)
(def usb-holder
  (->> (cube (+ (first usb-holder-size) usb-holder-thickness)
             (second usb-holder-size)
             (+ (last usb-holder-size) usb-holder-thickness))
       (translate [(first usb-holder-position)
                   (second usb-holder-position)
                   (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))
(def usb-holder-hole
  (->> (apply cube usb-holder-size)
       (translate [(first usb-holder-position)
                   (second usb-holder-position)
                   (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))


(def trrs-usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))

(def trrs-usb-holder-position
  (map + [17 19.3 0] [(first trrs-usb-holder-ref) (second trrs-usb-holder-ref) 2]))
(def trrs-usb-holder-cube
  (cube 15 12 2))
(def trrs-usb-holder-space
  (translate (map + trrs-usb-holder-position [0 (* -1 wall-thickness) 1]) trrs-usb-holder-cube))
(def trrs-usb-holder-holder
  (translate trrs-usb-holder-position (cube 19 12 4)))

(def trrs-usb-jack (translate (map + trrs-usb-holder-position [0 10 3]) (cube 8.1 20 3.1)))

(def trrs-holder-size [6.2 10 2]) ; trrs jack PJ-320A
(def trrs-holder-hole-size [6.2 10 6]) ; trrs jack PJ-320A
(def trrs-holder-position  (map + trrs-usb-holder-position [-13.6 0 0]))
(def trrs-holder-thickness 2)
(def trrs-holder-thickness-2x (* 2 trrs-holder-thickness))
(def trrs-holder
  (union
   (->> (cube (+ (first trrs-holder-size) trrs-holder-thickness-2x)
              (+ trrs-holder-thickness (second trrs-holder-size))
              (+ (last trrs-holder-size) trrs-holder-thickness))
        (translate [(first trrs-holder-position)
                    (second trrs-holder-position)
                    (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2)]))))
(def trrs-holder-hole
  (union
  ; circle trrs hole
   (->>
    (->> (binding [*fn* 30] (cylinder 2.55 20))) ; 5mm trrs jack
    (rotate (deg2rad  90) [1 0 0])
    (translate [(first trrs-holder-position)
                (+ (second trrs-holder-position)
                   (/ (+ (second trrs-holder-size) trrs-holder-thickness) 2))
                (+ 3 (/ (+ (last trrs-holder-size) trrs-holder-thickness) 2))])) ;1.5 padding
  ; rectangular trrs holder
   (->> (apply cube trrs-holder-hole-size)
        (translate [(first trrs-holder-position)
                    (+ (/ trrs-holder-thickness -2) (second trrs-holder-position))
                    (+ (/ (last trrs-holder-hole-size) 2) trrs-holder-thickness)]))))

(def pro-micro-position (map + (key-position 0 0.15 (wall-locate3 -1 0)) [-2 2 -30]))
(def pro-micro-space-size [4 10 12]) ; z has no wall;
(def pro-micro-wall-thickness 2)
(def pro-micro-holder-size
  [(+ pro-micro-wall-thickness (first pro-micro-space-size))
   (+ pro-micro-wall-thickness (second pro-micro-space-size))
   (last pro-micro-space-size)])
(def pro-micro-space
  (->> (cube (first pro-micro-space-size)
             (second pro-micro-space-size)
             (last pro-micro-space-size))
       (translate [(- (first pro-micro-position) (/ pro-micro-wall-thickness 2))
                   (- (second pro-micro-position) (/ pro-micro-wall-thickness 2))
                   (last pro-micro-position)])))
(def pro-micro-holder
  (difference
   (->> (cube (first pro-micro-holder-size)
              (second pro-micro-holder-size)
              (last pro-micro-holder-size))
        (translate [(first pro-micro-position)
                    (second pro-micro-position)
                    (last pro-micro-position)]))
   pro-micro-space))

(def teensy-width 20)
(def teensy-height 12)
(def teensy-length 33)
(def teensy2-length 53)
(def teensy-pcb-thickness 2)
(def teensy-holder-width  (+ 7 teensy-pcb-thickness))
(def teensy-holder-height (+ 6 teensy-width))
(def teensy-offset-height 5)
(def teensy-holder-top-length 18)
(def teensy-top-xy (key-position 0 (- centerrow 1) (wall-locate3 -1 0)))
(def teensy-bot-xy (key-position 0 (+ centerrow 1) (wall-locate3 -1 0)))
(def teensy-holder-length (- (second teensy-top-xy) (second teensy-bot-xy)))
(def teensy-holder-offset (/ teensy-holder-length -2))
(def teensy-holder-top-offset (- (/ teensy-holder-top-length 2) teensy-holder-length))

(def teensy-holder
  (->>
   (union
    (->> (cube 3 teensy-holder-length (+ 6 teensy-width))
         (translate [1.5 teensy-holder-offset 0]))
    (->> (cube teensy-pcb-thickness teensy-holder-length 3)
         (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-offset (- -1.5 (/ teensy-width 2))]))
    (->> (cube 4 teensy-holder-length 4)
         (translate [(+ teensy-pcb-thickness 5) teensy-holder-offset (-  -1 (/ teensy-width 2))]))
    (->> (cube teensy-pcb-thickness teensy-holder-top-length 3)
         (translate [(+ (/ teensy-pcb-thickness 2) 3) teensy-holder-top-offset (+ 1.5 (/ teensy-width 2))]))
    (->> (cube 4 teensy-holder-top-length 4)
         (translate [(+ teensy-pcb-thickness 5) teensy-holder-top-offset (+ 1 (/ teensy-width 2))])))
   (translate [(- teensy-holder-width) 0 0])
   (translate [-1.4 0 0])
   (translate [(first teensy-top-xy)
               (- (second teensy-top-xy) 1)
               (/ (+ 6 teensy-width) 2)])))

(defn screw-insert-shape [bottom-radius top-radius height]
  (union (cylinder [bottom-radius top-radius] height)
         (translate [0 0 (/ height 2)] (sphere top-radius))))

(defn screw-insert [column row bottom-radius top-radius height]
  (let [shift-right (= column lastcol)
        shift-left  (= column 0)
        shift-up    (and (not (or shift-right shift-left)) (= row 0))
        shift-down  (and (not (or shift-right shift-left)) (>= row lastrow))
        position    (if shift-up
                      (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                      (if shift-down
                        (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                        (if shift-left
                          (map + (left-key-position row 0) (wall-locate3 -1 0))
                          (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate [(first position) (second position) (/ height 2)]))))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (let [lastloc (if-not use-wide-pinky? (+ lastcol 0.1) (+ lastcol 0.5))]
    (union (screw-insert (if use-inner-column? -1 0)       0               bottom-radius top-radius height)
           (screw-insert (if use-inner-column? -1 0)       (- lastrow 0.8) bottom-radius top-radius height)
           (screw-insert 2       (+ lastrow 0.2) bottom-radius top-radius height)
           (screw-insert 3       0               bottom-radius top-radius height)
           #_(screw-insert lastloc 1               bottom-radius top-radius height))))
(def screw-insert-height 3.8)
(def screw-insert-bottom-radius (/ 5.31 2))
(def screw-insert-top-radius (/ 5.1 2))
(def screw-insert-holes
  (screw-insert-all-shapes screw-insert-bottom-radius
                           screw-insert-top-radius
                           screw-insert-height))
(def screw-insert-outers
  (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.6)
                           (+ screw-insert-top-radius 1.6)
                           (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def wire-post-height 7)
(def wire-post-overhang 3.5)
(def wire-post-diameter 2.6)
(defn wire-post [direction offset]
  (->> (union (translate [0 (* wire-post-diameter -0.5 direction) 0]
                         (cube wire-post-diameter wire-post-diameter wire-post-height))
              (translate [0 (* wire-post-overhang -0.5 direction) (/ wire-post-height -2)]
                         (cube wire-post-diameter wire-post-overhang wire-post-diameter)))
       (translate [0 (- offset) (+ (/ wire-post-height -2) 3)])
       (rotate (/ α -2) [1 0 0])
       (translate [3 (/ mount-height -2) 0])))

(def wire-posts
  (union
   (thumb-ml-place (translate [-5 0 -2] (wire-post  1 0)))
   (thumb-ml-place (translate [0 0 -2.5] (wire-post -1 6)))
   (thumb-ml-place (translate [5 0 -2] (wire-post  1 0)))
   (for [column (range 0 lastcol)
         row (range 0 cornerrow)]
     (union
      (key-place column row (translate [-5 0 0] (wire-post 1 0)))
      (key-place column row (translate [0 0 0] (wire-post -1 6)))
      (key-place column row (translate [5 0 0] (wire-post  1 0)))))))

(def model-right
  (difference
   (union
    key-holes
    (if use-inner-column? inner-key-holes ())
    (if show-caps? caps ())
    (if show-caps? thumbcaps ())
    pinky-connectors
    pinky-walls
    connectors
    thumb
    thumb-connectors
    (difference (union case-walls
                       #_screw-insert-outers
                       (if use-promicro-usb-hole?
                         (union pro-micro-holder
                                trrs-usb-holder-holder)
                         (union usb-holder
                                pro-micro-holder))
                       (if use-trrs? trrs-holder ()))
                (if use-promicro-usb-hole?
                  (union trrs-usb-holder-space
                         trrs-usb-jack)
                  usb-holder-hole)
                (if use-trrs? trrs-holder-hole rj9-space)
                #_screw-insert-holes)
    (if-not use-trrs? rj9-holder ())
    (if use-wire-post? wire-posts ()))
   (translate [0 0 -20] (cube 350 350 40))))

(spit "things/right.scad"
      (write-scad model-right))

(spit "things/left.scad"
      (write-scad (mirror [-1 0 0] model-right)))

(spit "things/right-test.scad"
      (write-scad
       (union
        key-holes
        connectors
        thumb
        thumb-connectors
        case-walls
        thumbcaps
        caps
        teensy-holder
        rj9-holder
        usb-holder-hole)))

(spit "things/right-plate.scad"
      (write-scad
       (cut
        (translate [0 0 -0.1]
                   (difference (union case-walls
                                      teensy-holder
                                          ; rj9-holder
                                      screw-insert-outers)
                               (translate [0 0 -10] screw-insert-screw-holes))))))

(spit "things/test.scad"
      (write-scad
       (difference usb-holder usb-holder-hole)))

(defn -main [dum] 1)  ; dummy to make it easier to batc
