(ns dactyl-keyboard.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [scad-clj.model :refer [pi]]
            [scad-clj.scad :refer [write-scad]]
            [selmer.parser :refer [render-file]]
            [dactyl-keyboard.dactyl :as dm]))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(defn parse-bool [s]
  (Boolean/valueOf s))

(defn generate-scad [confs]
  (write-scad (dm/model-right confs)))

(defn home [_]
  (render-file "index.html" {:name "water"}))

(defn generate [req]
  (let [params (:form-params req)
        param-ncols (get params "ncols")
        param-nrows (get params "nrows")
        param-trrs-connector (get params "trrs-connector")
        param-use-promicro-usb-hole (get params "usb-hole")
        param-side-nub (get params "side-nub")
        param-minidox (get params "minidox")
        param-rentalcar (get params "rentalcar")
        param-inner-column (get params "inner-column")
        param-last-row (get params "last-row")
        param-wide-pinky (get params "wide-pinky")
        param-wire-post (get params "wire-post")
        c (hash-map :configuration-nrows (parse-int param-nrows)
                    :configuration-ncols (parse-int param-ncols)
                    :configuration-alpha (/ pi 12)
                    :configuration-beta (/ pi 36)
                    :configuration-centercol 4
                    :configuration-tenting-angle (/ pi 12)
                    :configuration-create-side-nub? (parse-bool param-side-nub)
                    :configuration-minidox-style? (parse-bool param-minidox)
                    :configuration-rental-car? (parse-bool param-rentalcar)
                    :configuration-show-caps? false
                    :configuration-use-hotswap? false
                    :configuration-use-inner-column? (parse-bool param-inner-column)
                    :configuration-use-last-row? (parse-bool param-last-row)
                    :configuration-use-promicro-usb-hole? (parse-bool param-use-promicro-usb-hole)
                    :configuration-use-trrs? (parse-bool param-trrs-connector)
                    :configuration-use-wide-pinky? (parse-bool param-wide-pinky)
                    :configuration-use-wire-post? (parse-bool param-wire-post))
        generated-scad (generate-scad c)]
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