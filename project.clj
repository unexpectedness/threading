(defproject threading "0.1.6"
  :description "A threading macro library as sobber as its name"
  :url "https://github.com/unexpectedness/threading"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.clojars.unexpectedness/shuriken "0.14.1"]]
  :profiles {:dev  {:dependencies [[codox-theme-rdash "0.1.2"]]}}
  :plugins [[lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/unexpectedness/threading/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
