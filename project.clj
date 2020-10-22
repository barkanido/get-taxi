(defproject get-taxi "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [com.taoensso/timbre "5.1.0"]]
                 ;; [http-kit "2.5.0"]
                 ;; [metosin/jsonista "0.2.7"]
                 ;; [metosin/reitit "0.5.10"]]
  :repl-options {:init-ns get-taxi.core}
  :main get-taxi.core)
