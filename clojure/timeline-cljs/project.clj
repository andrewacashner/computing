(defproject timeline "0.1.0-SNAPSHOT"
  :description "Timeline game"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/clojurescript "1.11.54"]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 ]
  :main timeline.core
  :plugins [[lein-cljsbuild "1.1.7"]]

  :cljsbuild {:builds
              [{:id "default"
                :source-paths ["src"]
                :compiler {:optimizations :whitespace
                           :pretty-print true}}]})
