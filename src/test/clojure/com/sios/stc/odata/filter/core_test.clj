(ns com.sios.stc.odata.filter.core-test
  (:require [clojure.test :refer :all]
            [com.sios.stc.odata.filter.core :refer :all]
            [clojure.tools.logging
             :as log
             :refer [trace debug info warn error fatal
                     tracef debugf infof warnf errorf fatalf]])
  (:import (com.sios.stc.odata.filter ParseFailure)))

(deftest simple-predicate-test
  (testing "Nested predicate matching on java object"
    (let [match? (make-match-predicate "cause.message eq 'testme'")]
      (is (match? (Exception. (Exception. "testme")))))))

(deftest simple-empty-predicate-test
  (testing "Nested predicate matching on java object - make sure not empty compiles"
    (is (not (nil? (make-match-predicate "stuff is empty and not things is empty"))))))

(deftest like-predicate-test
  (testing "Nested predicate matching on java object with like"
    (let [match? (make-match-predicate "cause.message like 'estm'")]
      (is (match? (Exception. (Exception. "testme")))))))

(deftest like-predicate-test-with-wildcards ;; for predicates, nothing should happen
  (testing "Nested predicate matching on java object with like and incoming wildcards"
    (let [match? (make-match-predicate "cause.message like 'e%s_tm'")]
      (is (match? (Exception. (Exception. "te%s_tme")))))))

(deftest insensitive-like-predicate-test
  (testing "Nested predicate matching on java object with ilike"
    (let [match? (make-match-predicate "cause.message ilike 'Estm'")]
      (is (match? (Exception. (Exception. "testme")))))))

(deftest not-like-predicate-test
  (testing "Nested predicate matching on java object with like"
    (let [match? (make-match-predicate "not cause.message like 'estm' and cause.message like 'joe'")]
      (is (not (match? (Exception. (Exception. "testme"))))))))

(deftest insensitive-not-like-predicate-test
  (testing "Nested predicate matching on java object with ilike"
    (let [match? (make-match-predicate "not cause.message ilike 'Estm' and cause.message ilike 'joe'")]
      (is (not (match? (Exception. (Exception. "testme"))))))))

(deftest failed-like-predicate-test
  (testing "Invalid like expresion (only should work with string literals)"
    (is (thrown? ParseFailure (make-match-predicate "blah like 45")))
    (is (thrown? ParseFailure (make-query-info "blah like 45" "thing")))))

(deftest simple-query-test
  (testing "Simple query generation"
    (let [qinfo (make-query-info "id gt 5 and not things contain 42 and not tags is empty" "obj")]
      (is (not (nil? qinfo)))
      (is (= 2 (count (qinfo :param-map))))
      (is (re-matches #"^.*id > :.*$" (qinfo :where-clause)))
      (is (re-matches #"^.*exists.*select.*\.things.*where.*$"
                      (qinfo :where-clause))))))

(deftest simple-query-test-2
  (testing "Simple query generation"
    (let [qinfo (make-query-info "id gt 5 and not things ct 42 and not tags is empty" "obj")]
      (is (not (nil? qinfo)))
      (is (= 2 (count (qinfo :param-map))))
      (is (re-matches #"^.*id > :.*$" (qinfo :where-clause)))
      (is (re-matches #"^.*exists.*select.*\.things.*where.*$"
                      (qinfo :where-clause))))))

(deftest simple-query-test-with-contains-id
  (testing "Simple query generation with contains-id"
    (let [qinfo (make-query-info "id gt 5 and not things containsid 42 and not tags is empty" "obj")]
      (is (not (nil? qinfo)))
      (is (= 2 (count (qinfo :param-map))))
      (is (re-matches #"^.*id > :.*$" (qinfo :where-clause)))
      (is (re-matches #"^.*exists.*select.*\.things.*where.*$"
                      (qinfo :where-clause))))))

(deftest simple-query-test-with-contains-item
  (testing "Simple query generation with contains-item"
    (let [qinfo (make-query-info "id gt 5 and things containsitem 42 and not tags is empty" "obj")]
      (is (not (nil? qinfo)))
      (is (= 2 (count (qinfo :param-map))))
      (is (re-matches #"^.*id > :.*$" (qinfo :where-clause)))
      (is (re-matches #"^.* MEMBER OF obj.things.*$"
                      (qinfo :where-clause))))))

(deftest simple-query-test-with-not-contains-item
  (testing "Simple query generation with not contains-item"
    (let [qinfo (make-query-info "id gt 5 and not things containsitem 42 and not tags is empty" "obj")]
      (is (not (nil? qinfo)))
      (is (= 2 (count (qinfo :param-map))))
      (is (re-matches #"^.*id > :.*$" (qinfo :where-clause)))
      (is (re-matches #"^.* NOT MEMBER OF obj.things.*$"
                      (qinfo :where-clause))))))

(deftest like-query-test
  (testing "Like query generation"
    (let [qinfo (make-query-info "not name like 'kevin'" "obj")]
      (is (not (nil? qinfo)))
      (is (= 1 (count (qinfo :param-map))))
      (is (re-matches #"^.*NOT LIKE.*" (qinfo :where-clause)))
      (is (= "%kevin%" (second (first (qinfo :param-map))))))))

(deftest like-query-test-with-incoming-wildcards
  (testing "Like query generation with incoming wildcards"
    (let [qinfo (make-query-info "not name like 'kev_in%'" "obj")]
      (is (not (nil? qinfo)))
      (is (= 1 (count (qinfo :param-map))))
      (is (re-matches #"^.*NOT LIKE.*" (qinfo :where-clause)))
      (is (= "%kev\\_in\\%%" (second (first (qinfo :param-map))))))))

(deftest insensitive-like-query-test
  (testing "Insensitive like query generation"
    (let [qinfo (make-query-info "not name ilike 'Kevin'" "obj")]
      (is (not (nil? qinfo)))
      (is (= 1 (count (qinfo :param-map))))
      (is (re-matches #"^.*LOWER.*NOT LIKE.*LOWER.*" (qinfo :where-clause)))
      (is (= "%Kevin%" (second (first (qinfo :param-map))))))))

(deftest bool-query-test
  (testing "bool query generation"
    (let [qinfo (make-query-info "named eq True and active ne fALse" "obj")]
      (is (not (nil? qinfo)))
      (is (= 0 (count (qinfo :param-map)))) ;; bools get directly translated
      (is (re-matches #"^.*TRUE.*" (qinfo :where-clause)))
      (is (re-matches #"^.*FALSE.*" (qinfo :where-clause))))))

(deftest empty-string-query-test
  (testing "empty string query generation"
    (let [qinfo (make-query-info "not name eq ''" "obj")]
      (is (not (nil? qinfo)))
      (is (= 1 (count (qinfo :param-map))))
      (is (re-matches #"^.*NOT.+=.*" (qinfo :where-clause)))
      (is (= "" (second (first (qinfo :param-map))))))
    (let [qinfo (make-query-info "not name eq \"\"" "obj")]
      (is (not (nil? qinfo)))
      (is (= 1 (count (qinfo :param-map))))
      (is (re-matches #"^.*=.*" (qinfo :where-clause)))
      (is (= "" (second (first (qinfo :param-map))))))))

(deftest is-null-query-test
  (testing "is null query generation"
    (let [qinfo (make-query-info "name is null" "obj")]
      (is (not (nil? qinfo)))
      (is (= 0 (count (qinfo :param-map))))))) ;; nulls get directly translated

;; Note: These two null tests are expected to
;;       see the overall closing paren so the null
;;       clause needs to be at the end (there was a defect
;;       involving a missing paren in the generated clause).
(deftest not-is-null-query-test
  (testing "not (is null) query generation"
    (let [qinfo (make-query-info "five eq 5 and not (name is null)" "obj")]
      (is (not (nil? qinfo)))
      (is (.contains (qinfo :where-clause) "NOT (obj.name IS NULL))"))
      (is (= 1 (count (qinfo :param-map))))))) ;; 5 only actual param

(deftest is-not-null-query-test
  (testing "is not null query generation"
    (let [qinfo (make-query-info "five eq 5 and not name is null" "obj")]
      (is (not (nil? qinfo)))
      (is (.contains (qinfo :where-clause) "(obj.name IS NOT NULL))"))
      (is (= 1 (count (qinfo :param-map))))))) ;; 5 only actual param

(deftest literals-query-test
  (testing "literals query generation"
    (let [qinfo (make-query-info (str "one eq int'42'"
                                      " and two eq long'42'"
                                      " and three eq float'1.2'"
                                      " and four eq double'56.6'"
                                      " and five eq decimal'23.3'"
                                      " and six eq date'2014-06-12T13:03:03.023Z'"
                                      " and six eq dateascal'2014-06-12T13:03:03.023Z'"
                                      " and six eq datetime'2014-06-12T13:03:03.023Z'"
                                      " and six eq datetimeascal'2014-06-12T13:03:03.023Z'"
                                      " and six eq dateasdate'2014-06-12T13:03:03.023Z'"
                                      " and six eq datetimeasdate'2014-06-12T13:03:03.023Z'"
                                      " and seven eq enum'java.awt.Color/RED'")
                                 "thingie")]
      (is (not (nil? qinfo)))
      (is (= 12 (count (qinfo :param-map)))))))

(deftest literals-predicate-test
  (testing "literals predicate generation"
    (let [pred (make-match-predicate (str "one eq int'42'"
                                           " and two eq long'42'"
                                           " and three eq float'1.2'"
                                           " and four eq double'56.6'"
                                           " and five eq decimal'23.3'"
                                           " and six eq datetimeascal'2014-06-12T13:03:03.023Z'"
                                           " and six eq datetimeasdate'2014-06-12T13:03:03.023Z'"
                                           " and seven eq enum'java.awt.Color/RED'"))]
      (is (not (nil? pred))))))

(deftest simple-sort-clause-test
  (testing "A simple sort cause"
    (let [clause1 (make-sort-clause "prop1 asc" nil)
          clause2 (make-sort-clause "prop1" nil)
          clause3 (make-sort-clause "prop1,prop2,sub.prop3" nil)
          clause4 (make-sort-clause "prop1, prop2, sub.prop3 desc" nil)
          clause5 (make-sort-clause "prop1    ,   prop2  , sub.prop3     asc" nil)
          clause6 (make-sort-clause "prop1,prop2,sub.prop3 asc" "thingie")
          clause7 (make-sort-clause " \t prop1,prop2 desc,sub.prop3 asc  " "thingie")] ; with a little whitespace
      (is (= "prop1 ASC" clause1))
      (is (= "prop1" clause2))
      (is (= "prop1,prop2,sub.prop3" clause3))
      (is (= "prop1,prop2,sub.prop3 DESC" clause4))
      (is (= "prop1,prop2,sub.prop3 ASC" clause5))
      (is (= "thingie.prop1,thingie.prop2,thingie.sub.prop3 ASC" clause6))
      (is (= "thingie.prop1,thingie.prop2 DESC,thingie.sub.prop3 ASC" clause7)))))

(deftest simple-sort-comparator-test
  (testing "A simple sort comparator"
    (let [c (make-sort-comparator "p1, p2 desc")
          a {:p1 42 :p2 99}
          b {:p1 42 :p2 100}
          r (.compare c a b)]
      (is (= 1 r)))))

(deftest simple-sort-comparator-multdir-test
  (testing "A simple sort comparator with multiple directions"
    (let [c (make-sort-comparator "p1 desc, p2 asc")
          a1 {:p1 42 :p2 99}
          b1 {:p1 42 :p2 100}
          a2 {:p1 41 :p2 99}
          b2 {:p1 42 :p2 100}
          r1 (.compare c a1 b1)
          r2 (.compare c a2 b2)]
      (is (= -1 r1))
      (is (= 1 r2)))))

(deftest collection-sort-comparator-test
  (testing "A more complicated use of a sort comparator"
    (let [p1 (java.lang.ThreadGroup. "p1")
          p2 (java.lang.ThreadGroup. "p2")
          p3 (java.lang.ThreadGroup. "p3")
          b1 (java.lang.ThreadGroup. p1 "b1") ;; using threadgroup as a simple pojo
          b2 (java.lang.ThreadGroup. p2 "b2")
          b3 (java.lang.ThreadGroup. p3 "b3")
          l (java.util.ArrayList. [b3 b2 b1]) ;; need a mutable thing to sort
          c1 (make-sort-comparator " name\t") ;; with a little whitepace
          c2 (make-sort-comparator "maxPriority")
          c3 (make-sort-comparator "maxPriority, name desc")
          c4 (make-sort-comparator "parent.name asc")
          c5 (make-sort-comparator "parent.name, name desc")
          c6 (make-sort-comparator "parent.name desc, name")]
      (.setMaxPriority b1 (int 3)) ;; priority values in reverse
      (.setMaxPriority b2 (int 2))
      (.setMaxPriority b3 (int 1))
      (java.util.Collections/sort l c1)
      (is (= [b1 b2 b3] (vec l)))
      (java.util.Collections/sort l c2)
      (is (= [b3 b2 b1] (vec l)))
      (.setMaxPriority b1 (int 10)) ;; set pri to the same
      (.setMaxPriority b2 (int 10)) ;; to test sort by second field
      (.setMaxPriority b3 (int 10))
      (java.util.Collections/sort l c1) ;; and reset the order
      (java.util.Collections/sort l c3) ;; and try c3
      (is (= [b3 b2 b1] (vec l)))
      (java.util.Collections/sort l c4) ;; sub props reset the order?
      (is (= [b1 b2 b3] (vec l)))
      (java.util.Collections/sort l c5)
      (is (= [b1 b2 b3] (vec l))) ;; desc in c5 doesn't affect the first field
      (java.util.Collections/sort l c6)
      (is (= [b3 b2 b1] (vec l)))))) ;; desc in c6 works now

;; Try to ensure that
;; nested, matching parens aren't misintepreted
;; assertions are brute force checking of parens
;; since the predicate generation doesn't seem
;; to suffer.
;; (bug found on 03-15-2017 with actionState filters)
(deftest query-test-with-nested-parens
  (testing "Query generation with nested parens"
    (let [qinfo1 (make-query-info "((actionState eq 'Active') or (actionState is null)) and cloudEnvironment containsid true" "obj")
          qinfo2 (make-query-info "((actionState eq 'Active') or actionState is null) and cloudEnvironment containsid true" "obj")
          qinfo3 (make-query-info "(actionState eq 'Active' or (actionState is null)) and cloudEnvironment containsid true" "obj")
          qinfo4 (make-query-info "(actionState eq 'true' or (actionState is null)) and cloudEnvironment containsid true" "obj")
          nested (make-query-info "(( (actionState eq 'Active') ))" "it")]
      (log/debug "QUERY 1" qinfo1)
      (log/debug "QUERY 2" qinfo2)
      (log/debug "QUERY 3" qinfo3)
      (log/debug "QUERY NESTER EXPR" nested)
      (log/debug "QUERY TYPES 3" (map type (vals (qinfo3 :param-map))))
      (log/debug "QUERY TYPES 4" (map type (vals (qinfo4 :param-map))))
      (is (.startsWith (qinfo1 :where-clause) "((("))
      (is (.contains (qinfo1 :where-clause) "NULL)) AND (exists"))
      (is (.startsWith (qinfo2 :where-clause) "((("))
      (is (.contains (qinfo2 :where-clause) "NULL)) AND (exists"))
      (is (.startsWith (qinfo3 :where-clause) "((("))
      (is (.contains (qinfo3 :where-clause) "NULL)) AND (exists"))
      )))

;; Attempt to use the predicate feature to ensure that
;; nested, matching parens aren't misintepreted
;; but this doesn't work - they all pass
;; (deftest nested-matching-parens-test
;;   (testing "Nested parens via predicate"
;;     (let [match-lessparens? (make-match-predicate "(message eq 'testme' or message is null) and meat containsid true")
;;           match-moreparens? (make-match-predicate "((message eq 'testme') or (message is null)) and meat containsid true")]
;;       (letfn [(asserts [match?]
;;                 (is (match? {:message "testme" :meat [{:id true}]}))
;;                 (is (match? {:meat [{:id true}]}))              ; no message
;;                 (is (not (match? {:message "testme"})))  ; no meat
;;                 (is (not (match? {:message "testme" :meat [{:id "chicken"}]}))) ; bad meat
;;                 (is (not (match? {:meat [{:id "chicken"}]}))))] ; bad meat, no message
;;         (asserts match-lessparens?)
;;         (asserts match-moreparens?)))))

;; some snippets used during interactive sessions
;; (*filter-parser* "((actionState eq 'Active') or actionState is null) and cloudEnvironment contains true")
;; (*filter-parser* "((actionState eq 'Active') or actionState is null) and cloudEnvironment contains true")
;; (*filter-parser* "((actionState eq 'Active') or (actionState is null)) and cloudEnvironment contains true")
