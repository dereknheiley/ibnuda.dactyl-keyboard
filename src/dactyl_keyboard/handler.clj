(ns dactyl-keyboard.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [scad-clj.model :refer [pi]]
            [scad-clj.scad :refer [write-scad]]
            [selmer.parser :refer [render-file]]
            [dactyl-keyboard.manuform :as dm]
            [dactyl-keyboard.lightcycle :as dl])
  (:gen-class))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(defn parse-bool [s]
  (Boolean/valueOf s))

(defn generate-case-dl [confs is-right?]
  (write-scad (if is-right?
                (dl/dactyl-top-right confs)
                (dl/dactyl-top-left confs))))

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

(defn home [_]
  (render-file "index.html" {}))

(defn manuform [_]
  (render-file "manuform.html" {:column-curvature (range 12 22)
                                :tenting-angle (range 15 6 -1)
                                :thumb-tenting-angle (range 24 15 -1)
                                :height-offset (range 4 16 2)}))

(defn lightcycle [_]
  (render-file "lightcycle.html" {:column-curvature (range 12 22)
                                  :tenting-angle (range 12 6 -1)
                                  :thumb-tenting-angle (range 24 11 -1)
                                  :height-offset (range 10 32 2)}))

(defn generate-manuform [req]
  (let [p (:form-params req)
        param-ncols (parse-int (get p "ncols"))
        param-nrows (parse-int (get p "nrows"))
        param-minidox (parse-bool (get p "minidox"))
        param-last-row-count (case (get p "last-row")
                               "zero" :zero
                               "full" :full
                               :two)
        keyswitch-type (get p "switch-type")
        param-use-alps (case keyswitch-type "alps" true false)
        param-side-nub (case keyswitch-type "mx" true false)
        param-inner-column (parse-bool (get p "inner-column"))

        param-alpha (parse-int (get p "alpha"))
        param-beta (parse-int (get p "beta"))
        param-centercol (parse-int (get p "centercol"))
        param-tenting-angle (parse-int (get p "tenting-angle"))

        param-use-external-holder (parse-bool (get p "external-holder"))
        param-trrs-connector (parse-bool (get p "trrs-connector"))
        param-use-promicro-usb-hole (parse-bool (get p "usb-hole"))

        param-hotswap (parse-bool (get p "hotswap"))
        param-ortho (parse-bool (get p "ortho"))
        param-keyboard-z-offset (parse-int (get p "keyboard-z-offset"))
        param-wide-pinky (parse-bool (get p "wide-pinky"))
        param-wire-post (parse-bool (get p "wire-post"))
        param-screw-inserts (parse-bool (get p "screw-inserts"))
        param-show-keycaps (parse-bool (get p "show-keycaps"))
        param-wrist-rest (parse-bool (get p "wrist-rest"))
        param-integrated-wrist-rest (parse-bool (get p "integrated-wrist-rest"))
        is-right? (parse-bool (get p "right-side"))
        
        param-generate-plate (get p "generate-plate")
        param-generate-wrist-rest (get p "generate-wrist-rest")
        
        generate-plate? (some? param-generate-plate)
        generate-wrist-rest? (some? param-generate-wrist-rest)

        c {:configuration-nrows param-nrows
           :configuration-ncols param-ncols
           :configuration-minidox-style? param-minidox
           :configuration-last-row-count param-last-row-count
           :configuration-create-side-nub? param-side-nub
           :configuration-use-alps? param-use-alps
           :configuration-use-inner-column? param-inner-column

           :configuration-alpha (/ pi param-alpha)
           :configuration-beta (/ pi param-beta)
           :configuration-centercol param-centercol
           :configuration-tenting-angle (/ pi param-tenting-angle)

           :configuration-use-external-holder? param-use-external-holder
           :configuration-use-promicro-usb-hole?  param-use-promicro-usb-hole
           :configuration-use-trrs? param-trrs-connector

           :configuration-use-hotswap? param-hotswap
           :configuration-ortho? param-ortho
           :configuration-z-offset param-keyboard-z-offset
           :configuration-show-caps? param-show-keycaps
           :configuration-use-wide-pinky? param-wide-pinky
           :configuration-use-wire-post? param-wire-post
           :configuration-use-screw-inserts? param-screw-inserts
           :configuration-use-wrist-rest? param-wrist-rest
           :configuration-integrated-wrist-rest? param-integrated-wrist-rest}
        generated-scad (if generate-plate?
                         (generate-plate-dm c is-right?)
                         (if generate-wrist-rest?
                           (generate-wrist-rest-dm c is-right?)
                           (generate-case-dm c is-right?)))]
    {:status 200
     :headers {"Content-Type" "application/octet-stream"
               "Content-Disposition" "inline; filename=\"myfile.scad\""}
     :body generated-scad}))

(defn generate-lightcycle [req]
  (let [p (:form-params req)
        param-ncols (parse-int (get p "ncols"))
        param-use-numrow? (parse-bool (get p "num-row"))
        param-use-lastrow? (parse-bool (get p "last-row"))
        param-thumb-count (case (get p "thumb-count")
                            "2" :two
                            "3" :three
                            "6" :six
                            :five)
        param-alpha (parse-int (get p "alpha"))
        param-beta (parse-int (get p "beta"))
        param-tenting-angle (parse-int (get p "tenting-angle"))
        param-thumb-tenting-angle (parse-int (get p "thumb-tenting-angle"))
        param-z-offset (parse-int (get p "z-offset"))
        param-thumb-offset-x (parse-int (get p "thumb-offset-x"))
        param-thumb-offset-y (parse-int (get p "thumb-offset-y"))
        is-right? (parse-bool (get p "right-side"))
        param-thumb-offset-z (parse-int (get p "thumb-offset-z"))
        param-use-external-holder (parse-bool (get p "external-holder"))
        param-generate-plate (get p "generate-plate")
        generate-plate? (some? param-generate-plate)
        param-screw-inserts (parse-bool (get p "screw-inserts"))

        c {:configuration-ncols param-ncols
           :configuration-use-numrow? param-use-numrow?
           :configuration-use-lastrow? param-use-lastrow?
           :configuration-thumb-count param-thumb-count
           ; TODO: fix this
           :configuration-create-side-nub? false
           ; TODO: fix this
           :configuration-use-alps? false
           ; TODO: fix this
           :configuration-use-hotswap? false

           :configuration-alpha (/ pi param-alpha)
           :configuration-beta (/ pi param-beta)
           :configuration-tenting-angle (/ pi param-tenting-angle)
           :configuration-z-offset param-z-offset
           :configuration-thumb-tenting-angle (/ pi param-thumb-tenting-angle)
           :configuration-thumb-offset-x (- 0 param-thumb-offset-x)
           :configuration-thumb-offset-y (- 0 param-thumb-offset-y)
           :configuration-thumb-offset-z param-thumb-offset-z
           :configuration-use-external-holder? param-use-external-holder
           
           :configuration-use-screw-inserts? param-screw-inserts}
        generated-scad (if generate-plate?
                         (generate-plate-dl c is-right?)
                         (generate-case-dl c is-right?))]
    {:status 200
     :headers {"Content-Type" "application/octet-stream"
               "Content-Disposition" "inline; filename=\"myfile.scad\""}
     :body generated-scad}))

(defroutes app-routes
  (GET "/" [] home)
  (GET "/manuform" [] manuform)
  (POST "/manuform" [] generate-manuform)
  (GET "/lightcycle" [] lightcycle)
  (POST "/lightcycle" [] generate-lightcycle)
  (route/resources "/")
  (route/not-found "not found"))

(def app
  (wrap-defaults app-routes api-defaults))
