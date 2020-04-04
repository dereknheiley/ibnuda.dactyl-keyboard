(ns dactyl-keyboard.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [scad-clj.model :refer [pi]]
            [scad-clj.scad :refer [write-scad]]
            [selmer.parser :refer [render-file]]
            [dactyl-keyboard.dactyl :as dm])
  (:gen-class))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(defn parse-bool [s]
  (Boolean/valueOf s))

(defn generate-case [confs]
  (write-scad (dm/model-right confs)))

(defn generate-plate [confs]
  (write-scad (dm/right-plate confs)))

(defn home [_]
  (render-file "index.html" {:name "water"}))

(defn generate [req]
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
        param-generate-plate (get params "generate-plate")
        generate-plate? (some? param-generate-plate)
        c (hash-map :configuration-nrows param-nrows
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
                    :configuration-use-screw-inserts? param-screw-inserts)
        generated-scad (if generate-plate?
                         (generate-plate c)
                         (generate-case c))]
    {:status 200
     :headers {"Content-Type" "application/octet-stream"
               "Content-Disposition" "inline; filename=\"myfile.scad\""}
     :body generated-scad}))

(defroutes app-routes
  (GET "/" [] home)
  (POST "/" [] generate)
  (route/resources "/")
  (route/not-found "not found"))

(def app
  (wrap-defaults app-routes api-defaults))