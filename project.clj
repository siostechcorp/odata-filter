;;; Copyright (c) 2018 The odata-filter Authors (see AUTHORS file)
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

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
