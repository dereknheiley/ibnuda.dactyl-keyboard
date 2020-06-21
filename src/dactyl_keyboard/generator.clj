(ns dactyl-keyboard.generator
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.manuform :as dm]
            [dactyl-keyboard.lightcycle :as dl]))

(defn generate-case-dl [confs is-right?]
  (write-scad (if is-right?
                (dl/dactyl-top-right confs)
                (dl/dactyl-top-left confs))))

(defn generate-json-dm [confs is-right?]
  {:keys      {:columns      (get confs :configuration-ncols)
               :rows         (get confs :configuration-nrows)
               :thumb-count  (get confs :configuration-thumb-count)
               :last-row     (get confs :configuration-last-row-count)
               :nubs         (get confs :configuration-create-side-nub?)
               :alps         (get confs :configuration-use-alps?)
               :inner-column (get confs :configuration-use-inner-column?)}
   :curve     {:alpha     (get confs :configuration-alpha)
               :beta      (get confs :configuration-beta)
               :centercol (get confs :configuration-centercol)
               :tenting   (get confs :configuration-tenting-angle)}
   :connector {:external  (get confs :configuration-use-external-holder?)
               :trrs      (get confs :configuration-use-trrs?)
               :micro-usb (get confs :configuration-use-promicro-usb-hole?)}
   :form      {:hotswap       (get confs :configuration-use-hotswap?)
               :stagger       (not (get confs :configuration-ortho?))
               :wide-pinky    (get confs :configuration-use-wide-pinky?)
               :height-offset (get confs :configuration-z-offset)
               :wire-post     (get confs :configuration-use-wire-post?)
               :screw-inserts (get confs :configuration-use-screw-inserts?)}
   :misc      {:keycaps               (get confs :configuration-show-caps?)
               :wrist-rest            (get confs :configuration-use-wrist-rest?)
               :integrated-wrist-rest (get confs :configuration-integrated-wrist-rest?)
               :right-side            is-right?
               :case                  true}})

(defn generate-json-dl [confs is-right?]
  {:keys      {:columns     (get confs :configuration-ncols)
               :num-row     (get confs :configuration-use-numrow?)
               :last-row    (get confs :configuration-use-lastrow?)
               :thumb-count (get confs :configuration-thumb-count)}
   :curve     {:alpha         (get confs :configuration-alpha)
               :beta          (get confs :configuration-beta)
               :tenting       (get confs :configuration-tenting-angle)
               :thumb-alpha   (get confs :configuration-thumb-alpha)
               :thumb-beta    (get confs :configuration-thumb-beta)
               :thumb-tenting (get confs :configuration-thumb-tenting-angle)}
   :connector {:external (get confs :configuration-use-external-holder?)}
   :form      {:hotswap         (get confs :configuration-use-hotswap?)
               :thumb-offset-x  (get confs :configuration-thumb-offset-x)
               :thumb-offset-y  (get confs :configuration-thumb-offset-y)
               :thumb-offset-z  (get confs :configuration-thumb-offset-z)
               :z-offset        (get confs :configuration-z-offset)
               :manuform-offset (get confs :configuration-manuform-offset?)
               :border          (get confs :configuration-use-border?)}
   :misc      {:right-side    is-right?
               :screw-inserts (get confs :configuration-use-screw-inserts?)}})

(defn generate-plate-dl [confs is-right?]
  (write-scad (if is-right?
                (dl/dactyl-plate-right confs)
                (dl/dactyl-plate-left confs))))

(defn generate-case-dm [confs is-right?]
  (write-scad (if is-right?
                (dm/model-right confs)
                (dm/model-left confs))))

(defn generate-plate-dm [confs is-right?]
  (write-scad (if is-right?
                (dm/plate-right confs)
                (dm/plate-left confs))))

(defn generate-wrist-rest-dm [confs is-right?]
  (write-scad (if is-right?
                (dm/wrist-rest-right confs)
                (dm/wrist-rest-left confs))))
