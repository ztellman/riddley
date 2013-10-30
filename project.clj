(defproject riddley "0.1.5"
  :description "code-walking without caveats"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[codox "0.6.4"]]
  :codox {:writer codox-md.writer/write-docs
          :include [riddley.walk riddley.compiler]}
  :profiles {:dev {:dependencies [[codox-md "0.2.0" :exclusions [org.clojure/clojure]]]}}
  :java-source-paths ["src/riddley"]
  :javac-options ["-target" "1.5" "-source" "1.5"])
