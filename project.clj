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
                 [selmer "1.12.18"]]
  :plugins [[lein-exec "0.3.7"]
            [lein-auto "0.1.3"]
            [lein-ring "0.12.5"]]
  :ring {:handler dactyl-keyboard.handler/app}
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-mock "0.3.2"]]}})
