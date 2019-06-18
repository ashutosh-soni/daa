(defproject daa "0.1.0-SNAPSHOT"
  :description "design and analysis of algorithms"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :aot [daa.core]
  :main daa.core
  :test-src ["test"]
  :repl-options {:init-ns daa.core})
