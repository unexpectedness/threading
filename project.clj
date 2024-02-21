(defproject threading "0.4.0"
  :description "A threading macro library as sobber as its name"
  :url "https://github.com/unexpectedness/threading"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure                 "1.10.0"   :scope      "provided"]
                 [org.clojure/clojurescript           "1.10.520" :scope      "provided"]
                 [net.clojars.unexpectedness/shuriken "0.14.52"  :exclusions [threading]]]
  :plugins [[lein-codox     "0.10.8"]
            [lein-cljsbuild "1.1.8"]
            [lein-doo       "0.1.11"]]
  :prep-tasks ["compile" ["cljsbuild" "once"]]
  :cljsbuild {:builds {}}

  :profiles
  {:common  {:cljsbuild
             {:builds
              {:main {:source-paths ["src"]
                      :compiler     {:output-to     "target/js/main/threading.js"
                                     :output-dir    "target/js/main/"
                                     :optimizations :none}}}}}
   :dev     [:common
             {:dependencies
              [[doo "0.1.11"]
               [codox-theme-rdash "0.1.2"]]}]
   :test    [:dev
             {:cljsbuild
              {:builds
               {:main {:source-paths ["test"]
                       :compiler     {:target :nodejs
                                      :main   threading.doo-test
                                      :output-to  "target/js/test/threading.js"
                                      :output-dir "target/js/test/"}}}}}]
   :release [:common
             {:cljsbuild
              {:builds
               {:main {:jar      true
                       :compiler {:optimizations :advanced}}}}}]}
  
  :doo {:build "main"
        :alias {:default [:node]}}
  :aliases {"test"    ["with-profile" "test" ["do" ["test"] ["doo" "once"]]]
            "deploy"  ["with-profile" "release" "deploy"]
            "release" ["with-profile" "release" "release"]
            "doo"     ["with-profile" "test"    "doo"]}
  :codox {:source-uri "https://github.com/unexpectedness/threading/blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
