(defproject com.sios.stc/odata-filter
  "1.0.0-SNAPSHOT"
  :description "A library for converting odata-style filter expressions
into JPA/SQL style 'where' clauses and into in-memory predicate/comparator functions."
  :url "http://us.sios.com/"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :min-lein-version "2.0.0"
;;  :repositories ^:replace [["public" "http://.../nexus/content/groups/public"]
;;                           ["central" "http://.../nexus/content/groups/public"]
;;                           ["clojars" "http://.../nexus/content/groups/public"]
;;                           ["snapshots" {:url "http://.../nexus/content/repositories/snapshots/"
;;                                         :username "..."
;;                                         :password "..."}]
;;                           ["releases" {:url "http://.../nexus/content/repositories/releases/"
;;                                        :username "..."
;;                                        :password "..."}]]
;; should work with clojure 1.9, just not required here
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.memoize "0.7.1"]
                 [org.clojure/java.data "0.1.1"]
                 [org.clojure/tools.logging "0.4.0"]
                 [instaparse "1.4.9"]
                 [clj-time "0.14.3"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]
  :repl-options {:init-ns com.sios.stc.odata.filter.core}
  :aot [com.sios.stc.odata.filter.ParseFailure
        com.sios.stc.odata.filter.protocols
        com.sios.stc.odata.filter.core]
  :main com.sios.stc.odata.filter.core
  ;; :auto-clean false
  ;; :deploy-branches ["master"] ;; just to be safe - don't use with jenkins
  :plugins [[codox "0.8.13"]]
  :codox {:output-dir "target/site/apidocs"}
  ;; :parent [com.sios.stc/...]
  ;;         "..."
  ;;         :relative-path "../pom.xml"]
  )
