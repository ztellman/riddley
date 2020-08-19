(defproject riddley "0.2.1-SNAPSHOT"
  :description "code-walking without caveats"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies []
  :plugins [[lein-codox "0.9.4"]]
  :codox {:src-dir-uri "https://github.com/ztellman/riddley/tree/master/"
          :src-linenum-anchor-prefix "L"
          :defaults {:doc/format :markdown}
          :include [riddley.walk riddley.compiler]
          :output-dir "doc"}
  :profiles {:provided {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :dev {:dependencies [[org.clojure/clojure "1.10.1"]
                                  [pjstadig/humane-test-output "0.10.0"]]
                   :injections   [(require 'pjstadig.humane-test-output)
                                  (pjstadig.humane-test-output/activate!)]}}
  :java-source-paths ["src/riddley"]
  :javac-options ["-target" "1.7" "-source" "1.7"])
