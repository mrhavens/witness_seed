(defproject witness-seed "0.1.0-SNAPSHOT"
  :description "Witness Seed 2.0: Collaborative Storytelling Engine Edition"
  :license {:name "CC BY-NC-SA 4.0"
            :url "https://creativecommons.org/licenses/by-nc-sa/4.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [http-kit "2.7.0"]
                 [org.clojure/core.async "1.6.681"]
                 [cheshire "5.12.0"]]
  :main ^:skip-aot witness-seed.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})