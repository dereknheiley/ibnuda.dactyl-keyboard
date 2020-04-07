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

(defn generate-case-dl [confs]
  (write-scad (dl/dactyl-top-right confs)))

(defn generate-case-dm [confs]
  (write-scad (dm/model-right confs)))

(defn generate-plate-dm [confs]
  (write-scad (dm/right-plate confs)))

(defn home [_]
  (render-file "index.html" {}))

(defn manuform [_]
  (render-file "manuform.html" {}))

(defn lightcycle [_]
  (render-file "lightcycle.html" {}))

(defn generate-manuform [req]
  (let [params (:form-params req)
        param-ncols (parse-int (get params "ncols"))
        param-nrows (parse-int (get params "nrows"))
        param-minidox (parse-bool (get params "minidox"))
        param-last-row-count (case (get params "last-row")
                               "zero" :zero
                               "full" :full
                               :two)
        keyswitch-type (get params "switch-type")
        param-use-alps (case keyswitch-type "alps" true false)
        param-side-nub (case keyswitch-type "mx" true false)
        param-inner-column (parse-bool (get params "inner-column"))

        param-alpha (parse-int (get params "alpha"))
        param-beta (parse-int (get params "beta"))
        param-centercol (parse-int (get params "centercol"))
        param-tenting-angle (parse-int (get params "tenting-angle"))

        param-trrs-connector (parse-bool (get params "trrs-connector"))
        param-use-promicro-usb-hole (parse-bool (get params "usb-hole"))

        param-hotswap (parse-bool (get params "hotswap"))
        param-ortho (parse-bool (get params "ortho"))
        param-keyboard-z-offset (parse-int (get params "keyboard-z-offset"))
        param-wide-pinky (parse-bool (get params "wide-pinky"))
        param-wire-post (parse-bool (get params "wire-post"))
        param-screw-inserts (parse-bool (get params "screw-inserts"))
        param-show-keycaps (parse-bool (get params "show-keycaps"))
        param-wrist-rest (parse-bool (get params "wrist-rest"))
        param-generate-plate (get params "generate-plate")
        generate-plate? (some? param-generate-plate)
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

           :configuration-use-promicro-usb-hole?  param-use-promicro-usb-hole
           :configuration-use-trrs? param-trrs-connector

           :configuration-use-hotswap? param-hotswap
           :configuration-ortho? param-ortho
           :configuration-keyboard-z-offset param-keyboard-z-offset
           :configuration-show-caps? param-show-keycaps
           :configuration-use-wide-pinky? param-wide-pinky
           :configuration-use-wire-post? param-wire-post
           :configuration-use-screw-inserts? param-screw-inserts
           :configuration-use-wrist-rest? param-wrist-rest}
        generated-scad (if generate-plate?
                         (generate-plate-dm c)
                         (generate-case-dm c))]
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
                            :five)
        param-alpha (parse-int (get p "alpha"))
        param-beta (parse-int (get p "beta"))
        param-tenting-angle (parse-int (get p "tenting-angle"))
        param-thumb-tenting-angle (parse-int (get p "thumb-tenting-angle"))
        param-z-offset (parse-int (get p "z-offset"))
        param-thumb-offset-x (parse-int (get p "thumb-offset-x"))
        param-thumb-offset-y (parse-int (get p "thumb-offset-y"))
        param-thumb-offset-z (parse-int (get p "thumb-offset-z"))
        c {:configuration-ncols param-ncols
           :configuration-use-numrow? param-use-numrow?
           :configuration-use-lastrow? param-use-lastrow?
           :configuration-thumb-count param-thumb-count

           :configuration-alpha (/ pi param-alpha)
           :configuration-beta (/ pi param-beta)
           :configuration-tenting-angle (/ pi param-tenting-angle)
           :configuration-z-offset param-z-offset
           :configuration-thumb-tenting-angle (/ pi param-thumb-tenting-angle)
           :configuration-thumb-offset-x (- 0 param-thumb-offset-x)
           :configuration-thumb-offset-y (- 0 param-thumb-offset-y)
           :configuration-thumb-offset-z param-thumb-offset-z}
        generated-scad (generate-case-dl c)]
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