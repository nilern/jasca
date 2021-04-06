(defproject jasca "0.1.0-SNAPSHOT"
  :description "JavAScript object notation parser CombinAtors"
  :url "https://github.com/nilern/jasca"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-jmh "0.3.0"]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.fasterxml.jackson.core/jackson-core "2.11.2"]
                 [com.deepbeginnings/monnit "0.1.2"]]
  :repl-options {:init-ns jasca.core}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.1"]
                                  [jmh-clojure/jmh-clojure "0.4.0"]
                                  [com.fasterxml.jackson.datatype/jackson-datatype-joda "2.11.2"]
                                  [metosin/jsonista "0.2.7"]
                                  [cheshire "5.10.0"]
                                  [org.clojure/data.json "1.0.0"]
                                  [com.cognitect/transit-clj "1.0.324"]
                                  [criterium "0.4.6"]]
                   :global-vars {*warn-on-reflection* true}}
             :jmh {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :perf {:jvm-opts ^:replace ["-server"
                                         "-Xmx4096m"
                                         "-Dclojure.compiler.direct-linking=true"]}}
  :aliases {"perf" ["with-profile" "default,dev,perf"]})

