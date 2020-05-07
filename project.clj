(defproject dactyl-keyboard "0.1.0-SNAPSHOT"
  :description "A parametrized, split-hand, concave, columnar, erogonomic keyboard"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [unicode-math "0.2.0"]
                 [scad-clj "0.5.3"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-jetty-adapter "1.8.0"]
                 [selmer "1.12.18"]]
  :plugins [[lein-exec "0.3.7"]
            [lein-auto "0.1.3"]
            [lein-ring "0.12.5"]]
  :aliases {"generate-lightcycle" ["exec" "-p" "src/dactyl_keyboard/lightcycle.clj"]
            "generate-manuform" ["exec" "-p" "src/dactyl_keyboard/manuform.clj"]}
  :ring {:handler dactyl-keyboard.handler/app
         :port 3030
         :auto-reload? true}
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.3.2"]]}})
