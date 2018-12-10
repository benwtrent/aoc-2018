(defproject advent-of-code-2018 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :jvm-opts ["-Xmx2g" "-Xms2g"]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/math.combinatorics "0.1.4"]
                                  [org.clojure/math.numeric-tower "0.0.4"]]}}
  :main advent-of-code-2018.core)
