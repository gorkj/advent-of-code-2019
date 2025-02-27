(defproject advent-of-code-2019 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.taoensso/tufte "2.1.0"]
                 [com.taoensso/truss "1.5.0"]
                 [aysylu/loom "1.0.2"]]
  :main ^:skip-aot advent-of-code-2019.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
