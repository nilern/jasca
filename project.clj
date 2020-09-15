(defproject jasca "0.1.0-SNAPSHOT"
  :description "JavAScript object notation parser CombinAtors"
  :url "https://github.com/nilern/jasca"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.fasterxml.jackson.core/jackson-core "2.11.2"]]
  :repl-options {:init-ns jasca.core})
