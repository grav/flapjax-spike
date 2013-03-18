(defproject flapjax-spike "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.0.4"]
                 [hiccup "1.0.0"]
                 [ring-edn "0.1.0"]]
  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-ring "0.7.0"]]
  :cljsbuild {:builds
              [{:source-paths ["src/cljs"],
                :compiler
                {:pretty-print true,
                 :output-to "resources/public/js/main.js",
                 :foreign-libs
                 [{:provides "flapjax",
                   :file "http://www.flapjax-lang.org/download/flapjax-2.1.js"}],
                 :optimizations :whitespace}}]}
  :ring {:handler flapjax-spike.routes/app})
