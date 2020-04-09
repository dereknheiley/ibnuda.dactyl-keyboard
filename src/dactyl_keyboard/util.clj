(ns dactyl-keyboard.util
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.common :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 0)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

;; takes a list of 'location's,
;; partitions them into triad,
;; and apply hull on each triad,
;; then finally apply union for the result.
(defn triangle-hulls
  "It creates a wall that borders with the 'location's in the list."
  [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn frow-radius
  "It computes the radius of the row's curve. It takes the value of `pi` divided
   by `alpha` to compute the said radius."
  [alpha]
  (+ (/ (/ (+ mount-height extra-height) 2)
        (Math/sin (/ alpha 2)))
     cap-top-height))

(defn fcolumn-radius
  "It computes the radius of the column's curve. It takes the value of `pi` divided
   by `beta` to compute the said radius."
  [beta]
  (+ (/ (/ (+ mount-width extra-width) 2)
        (Math/sin (/ beta 2)))
     cap-top-height))
