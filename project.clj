(defproject prolog-analyzer "0.1.2"
  :description "A library for analyzing clojure code"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [lein-githooks "0.1.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [ubergraph "0.5.2"]]
  :main ^:skip-aot prolog-analyzer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]
                   :plugins [[lein-githooks "0.1.0"]]
                   :githooks {:auto-install true
                              :pre-push ["lein eastwood"]
                              :pre-commit ["lein eastwood"]}}})
