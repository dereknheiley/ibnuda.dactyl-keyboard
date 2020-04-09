(ns dactyl-keyboard.common
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

; common parts between the two boards.

(def extra-width 2.5)
(def extra-height 1.0)

; Was 14.1, then 14.25
(def keyswitch-height 14.0)
(def keyswitch-width 14.0)

(def alps-width 15.6)
(def alps-notch-width 15.5)
(def alps-notch-height 1)
(def alps-height 13)

(def sa-profile-key-height 12.7)

(def plate-thickness 5)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def cap-top-height (+ plate-thickness sa-profile-key-height))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

; each and every single switch hole is defined by this function.
(defn single-plate
  "Defines the form of switch hole. It determines the whether it uses
   box or mx style based on the `configuration-create-side-nub?`. It also
   asks whether it creates hotswap housing or not based on `configuration-use-hotswap?`.
   and determines whether it should use alps cutout or not based on  `configuration-use-alps?`"
  [c]
  (let [create-side-nub? (get c :configuration-create-side-nub?)
        use-hotswap? (get c :configuration-use-hotswap?)
        use-alps? (get c :configuration-use-alps?)
        top-wall (if use-alps?
                   (->> (cube (+ keyswitch-width 3) 2.2 plate-thickness)
                        (translate [0
                                    (+ (/ 2.2 2) (/ alps-height 2))
                                    (/ plate-thickness 2)]))
                   (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                        (translate [0
                                    (+ (/ 1.5 2) (/ keyswitch-height 2))
                                    (/ plate-thickness 2)])))
        left-wall (if use-alps?
                    (union (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                                (translate [(+ (/ 1.5 2) (/ 15.6 2))
                                            0
                                            (/ plate-thickness 2)]))
                           (->> (cube 1.5 (+ keyswitch-height 3) 1.0)
                                (translate [(+ (/ 1.5 2) (/ alps-notch-width 2))
                                            0
                                            (- plate-thickness
                                               (/ alps-notch-height 2))])))
                    (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                         (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                     0
                                     (/ plate-thickness 2)])))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ pi 2) [1 0 0])
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
                       (if (and use-hotswap?
                                (not use-alps?))
                         hotswap-holder
                         ())))))
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

(def web-thickness 5)
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

;; connectors
(def rj9-cube
  (cube 14.78 13 22.38))
(defn rj9-position
  "TODO: doc"
  [frj9-start c]
  [(first (frj9-start c)) (second (frj9-start c)) 11])
(defn rj9-space
  "TODO: doc"
  [frj9-start c]
  (translate (rj9-position frj9-start c) rj9-cube))
(defn rj9-holder
  "TODO: doc"
  [frj9-start c]
  (translate
   (rj9-position frj9-start c)
   (difference rj9-cube
               (union (translate [0 2 0] (cube 10.78  9 18.38))
                      (translate [0 0 5] (cube 10.78 13  5))))))

(def usb-holder-size [6.5 10.0 13.6])
(def usb-holder-thickness 4)
(defn usb-holder
  "TODO: doc"
  [fusb-holder-position c]
  (->> (cube (+ (first usb-holder-size) usb-holder-thickness)
             (second usb-holder-size)
             (+ (last usb-holder-size) usb-holder-thickness))
       (translate [(first (fusb-holder-position c))
                   (second (fusb-holder-position c))
                   (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))
(defn usb-holder-hole
  "TODO: doc"
  [fusb-holder-position c]
  (->> (apply cube usb-holder-size)
       (translate [(first (fusb-holder-position c))
                   (second (fusb-holder-position c))
                   (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))

(defn screw-insert-shape [bottom-radius top-radius height]
  (union (cylinder [bottom-radius top-radius] height)
         (translate [0 0 (/ height 2)] (sphere top-radius))))

(def screw-insert-height 3.8)
(def screw-insert-bottom-radius (/ 5.31 2))
(def screw-insert-top-radius (/ 5.1 2))

(defn screw-insert-holes
  "TODO: doc.
   but basically it takes a function that places screw holes with the following specs."
  [placement-function c]
  (placement-function c
                      screw-insert-bottom-radius
                      screw-insert-top-radius
                      screw-insert-height))
(defn screw-insert-outers
  "TODO: doc.
   but basically it takes a function that places outer parts of screw holes with the following specs."
  [placement-function c]
  (placement-function c
                      (+ screw-insert-bottom-radius 1.6)
                      (+ screw-insert-top-radius 1.6)
                      (+ screw-insert-height 1.5)))
(defn screw-insert-screw-holes
  "TODO: doc."
  [placement-function c]
  (placement-function c 1.7 1.7 350))

(def wrist-rest-back-height 29)
(def wrist-rest-angle 0)
(def wrist-rest-rotation-angle 0)
(def wrist-rest-ledge 3.5)
(defn wrist-rest-y-angle [tenting-angle] (* tenting-angle 45))

;;Wrist rest to case connections
(defn left-wrist-connector-x [ncols] (if (> ncols 5) -40 -40))
(defn middle-wrist-connector-x [ncols] (if (> ncols 5) -12 -17.5))
(defn right-wrist-connector-x [ncols] (if (> ncols 5) 24 5))
(def wrist-right-nut-y 20.5)
(def wrist-base-position-x -1)

(def wrist-rest-front-cut
  (scale [1.1, 1, 1]
         (->> (cylinder 7 200)
              (with-fn 50)
              (translate [0 -13.4 0]))))

(def cut-bottom
  (->> (cube 300 300 100) (translate [0 0 -50])))

(def h-offset
  (* (Math/tan (/ (* pi wrist-rest-angle) 180)) 88))

(def scale-cos
  (Math/cos (/ (* pi wrist-rest-angle) 180)))

(def scale-amount
  (/ (* 83.7 scale-cos) 19.33))

(def wrist-rest
  (difference
   (scale [4.25 scale-amount 1]
          (difference
           (union
            (difference
             (scale [1.3, 1, 1]
                    (->> (cylinder 10 150)
                         (with-fn 50)
                         (translate [0 0 0])))
             (scale [1.1, 1, 1]
                    (->> (cylinder 7 201)
                         (with-fn 50)
                         (translate [0 -13.4 0]))
                    (->> (cube 18 10 201)
                         (translate [0 -12.4 0]))))
            (->> (cylinder 6.8 200)
                 (with-fn 50)
                 (translate [-6.15 -0.98 0]))
            (->> (cylinder 6.8 200)
                 (with-fn 50)
                 (translate [6.15 -0.98 0]))
            (->> (cylinder 5.9 200)
                 (with-fn 50)
                 (translate [-6.35 -2 0]))
            (scale [1.01, 1, 1]
                   (->> (cylinder 5.9 200)
                        (with-fn 50)
                        (translate [6.35 -2. 0]))))))
   cut-bottom))

(defn wrist-rest-base [c]
  (let [tenting-angle (get c :configuration-tenting-angle)]
    (->>
     (scale [1 1 1] ;;;;scale the wrist rest to the final size after it has been cut
            (difference
             (scale [1.08 1.08 1] wrist-rest)
             (->> (cube 200 200 200)
                  (translate [0 0 (+ (+ (/ h-offset 2)
                                        (- wrist-rest-back-height h-offset))
                                     100)])
                  (rotate  (/ (* pi wrist-rest-angle) 180)  [1 0 0])
                  (rotate  (/ (* pi (wrist-rest-y-angle tenting-angle)) 180)  [0 1 0]))
             (->> (difference
                   wrist-rest
                   (->> (cube 200 200 200)
                        (translate [0 0 (- (+ (/ h-offset 2)
                                              (- wrist-rest-back-height h-offset))
                                           (+ 100  wrist-rest-ledge))])
                        (rotate (/ (* pi wrist-rest-angle) 180) [1 0 0])
                        (rotate (/ (* pi (wrist-rest-y-angle tenting-angle)) 180)  [0 1 0])))))))))

(defn rest-case-cuts [c]
  (let [ncols (get c :configuration-ncols)
        nrows (get c :configuration-nrows)]
    (union
     (->> (cylinder 1.85 25)
          (with-fn 30)
          (rotate  (/ pi 2)  [1 0 0])
          (translate [(right-wrist-connector-x ncols) 24 4.5]))
     (->> (cylinder 2.8 5.2)
          (with-fn 50)
          (rotate  (/ pi 2)  [1 0 0])
          (translate [(right-wrist-connector-x ncols) (+ 33.8 nrows) 4.5]))
     (->> (cube 6 3 12.2)
          (translate [(right-wrist-connector-x ncols) (+ wrist-right-nut-y nrows) 1.5]))
     (->> (cylinder 1.85 25)
          (with-fn 30)
          (rotate (/ pi 2)  [1 0 0])
          (translate [(middle-wrist-connector-x ncols) 14 4.5]))
     (->> (cylinder 2.8 5.2)
          (with-fn 50)
          (rotate (/ pi 2) [1 0 0])
          (translate [(middle-wrist-connector-x ncols) 26 4.5]))
     (->> (cube 6 3 12.2)
          (translate [(middle-wrist-connector-x ncols) (+ 10.0 nrows) 1.5]))
     (->> (cylinder 1.85 25)
          (with-fn 30)
          (rotate (/ pi 2) [1 0 0])
          (translate [(left-wrist-connector-x ncols) 11 4.5]))
     (->> (cylinder 2.8 5.2)
          (with-fn 50)
          (rotate (/ pi 2) [1 0 0])
          (translate [(left-wrist-connector-x ncols) (+ 17.25 nrows) 4.5]))
     (->> (cube 6 3 12.2)
          (translate [(left-wrist-connector-x ncols) (+ 6.0 nrows) 1.5])))))

(defn rest-case-connectors [c]
  (let [ncols (get c :configuration-ncols)]
    (difference
     (union
      (scale [1 1 1.6]
             (->> (cylinder 8 60)
                  (with-fn 50)
                  (rotate  (/ pi 2) [1 0 0])
                  (translate [(right-wrist-connector-x ncols) 14 4])))
      (scale [1 1 1.6]
             (->> (cylinder 8 60)
                  (with-fn 50)
                  (rotate (/ pi 2) [1 0 0])
                  (translate [(middle-wrist-connector-x ncols) 19 4])))
      (scale [1 1 1.6]
             (->> (cylinder 8 60)
                  (with-fn 50)
                  (rotate (/ pi 2) [1 0 0])
                  (translate [(left-wrist-connector-x ncols) 14 4])))))))
