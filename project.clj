(defproject prolog-analyzer "1.0.0"
  :description "A library for analyzing clojure code"
  :url "https://github.com/isabelwingen/prolog-analyzer"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.logging "0.4.1"]
                 [hiccup "1.0.5"]
                 [midje "1.9.9"]
                 [lein-midje "3.2.1"]
                 [instaparse "1.4.10"]
                 [orchestra "2018.12.06-2"]
                 [org.flatland/ordered "1.5.7"]
                 [org.clojure/data.json "0.2.6"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [ubergraph "0.5.2"]
                 [tableflisp "0.1.0"]]
  :main ^:skip-aot prolog-analyzer.core
  :target-path "target/%s"
  :jvm-opts ["-Xms2g" "-Xmx6g"]
  :profiles {:dev {:dependencies [[midje "1.9.9"]]
                   :plugins [[lein-midje "3.2.1"]]}
             :uberjar {:aot :all}})
