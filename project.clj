(defproject prolog-analyzer "0.2.0"
  :description "A library for analyzing clojure code"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [lein-githooks "0.1.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [ubergraph "0.5.2"]
                 [tableflisp "0.1.0"]]
  :plugins [[jonase/eastwood "0.3.5"]]
  :main ^:skip-aot prolog-analyzer.core
  :target-path "target/%s"
  :jvm-opts ["-Xms2g" "-Xmx6g"]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]
                   :plugins []}})
