(defproject my-stuff "0.1.0-SNAPSHOT"
  :description "Following Phil's Leiningen tutorial"
  :url "https://github.com/technomancy/leiningen/blob/master/doc/TUTORIAL.md"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.apache.lucene/lucene-core "3.0.2"]
                 [lancet "1.0.0"]]
  :profiles {:dev {:dependencies [[midje "1.3.1"]]}}
  :test-selectors {:default (complement :integration)
                  :integration :integration
                  :all (fn [_] true)}
  :main toot.core)