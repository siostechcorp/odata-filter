(ns com.sios.stc.odata.filter.core
  (:require [clojure.core.memoize :as memo :refer [lru]]
            [clojure.string :as str :refer [split lower-case]]
            [clojure.java.io :as io :refer [resource]]
            [clojure.java.data :as data :refer [from-java]]
            [instaparse.core :as iparse :refer [parser transform
                                                failure? get-failure]]
            [clj-time.format :as ctf]
            [clojure.tools.logging
             :as log
             :refer [trace debug info warn error fatal
                     tracef debugf infof warnf errorf fatalf]])
  (:import (com.sios.stc.odata.filter ParseFailure))
  (:gen-class
   :name com.sios.stc.odata.filter.core
   :methods [#^{:static true}
             [makeMatchPredicate [java.lang.String] clojure.lang.IFn]
             #^{:static true}
             [makeQueryInfo [java.lang.String] java.util.Map]
             #^{:static true}
             [makeQueryInfo [java.lang.String java.lang.String] java.util.Map]
             #^{:static true}
             [makeSortComparator [java.lang.String] java.util.Comparator]
             #^{:static true}
             [makeSortClause [java.lang.String] java.lang.String]
             #^{:static true}
             [makeSortClause [java.lang.String java.lang.String] java.lang.String]]
   ))

(def memo-cache-size 4096)

;; This is an atom (requiring deref for the value)
;; to match usage even when dynamically bound with a different atom
(def ^{:doc "The sequence of names to use for *QL parameters.  (lazy, infinite)"
       :dynamic true}
  *param-tags* (atom (map #(str "p_" %) (iterate inc 0))))

(def ^{:doc "The default datetime formatter/parser for typical use by this library."
       :dynamic true}
  *date-format* (ctf/formatters :date-time-parser))

(defn make-parser
  "Make a parser for odata expressions that returns an AST."
  [res]
  (infof "Generating odata expression parser from '%s'" res)
  (let [p (parser (resource res))]
    (infof "Parser generated for '%s'" res)
    p))

(def ^{:doc "The default odata filter parser for typical use by this library."
       :dynamic true}
  *filter-parser* (make-parser "filter.bnf"))

(def ^{:doc "The default odata sort parser for typical use by this library."
       :dynamic true}
  *sort-parser* (make-parser "sort.bnf"))

(defn property-value
  "Extracts a nested property from a clojure map where the
property is in string form like 'thing.sub.prop'.

This expects a map so use something like clojure.java.data/from-java
first to handle arbitrary types if needed."
  [m propstr]
  (debugf "Looking for property '%s' in %s" propstr m)
  (get-in m (map keyword (split propstr #"\."))))

(defn compare-via-props
  "Compare a with b but only via the properties in the list,
in order (first wins).

The props argument is expected to be a seq containing
a property name and an optional modifier function for
the direction of the comparison (+/- usually).

And the properties being compared must actually be
Comparable."
  [a b props]
  (let [amap (from-java a)
        bmap (from-java b)
        redf (fn [a [i & ms]] ;; once a is non-0, just pass through (so first wins)
               (if (= a 0)
                 (let [modifier (if (empty? ms) #'+ (first ms))
                       aprop (property-value amap i)
                       bprop (property-value bmap i)]
                   (modifier (compare aprop bprop)))
                 a))]
    (reduce redf 0 props)))

(defn ^java.util.Comparator make-comparator
  "Given a comparison function, make it into a comparator implementation."
  [f]
  (reify java.util.Comparator
    (compare [this a b] (f a b))))

(defn rewrite-expr-as-s-expression
  "Rewrites an expression (logical, comparison, etc.) as a prefix s-expression."
  ([expr]
     expr)
  ([not expr]
     `(and (not (nil? ~expr)) (~not ~expr))) ; nil checks on lhs's to avoid npes
  ([lhs op rhs]
     (cond
      (= :contain op) `(and (not (nil? ~lhs))
                            ;; any items where lhs.id = rhs? (want at least one)
                            (not (empty? (filter #(= % ~rhs) (map :id ~lhs)))))
      (= :containid op) `(and (not (nil? ~lhs))
                              ;; any items where lhs.id = rhs? (want at least one)
                              (not (empty? (filter #(= % ~rhs) (map :id ~lhs)))))
      (= :containitem op) `(and (not (nil? ~lhs))
                                ;; any items where lsh = rhs? (want at least one)
                                (not (empty? (filter #(= % ~rhs) ~lhs))))
      (= :like op) `(and (not (nil? ~lhs))
                         (.contains ~lhs ~rhs))
      (= :ilike op) `(and (not (nil? ~lhs))
                          (.contains (lower-case ~lhs) (lower-case ~rhs)))
      :else `(and (not (nil? ~lhs))
                  (~op ~lhs ~rhs))))
  ([not lhs op rhs]
   (cond
     (= :contain op) `(and (not (nil? ~lhs))
                           ;; any items where lhs.id = rhs? (want none)
                           (empty? (filter #(= % ~rhs) (map :id ~lhs))))
     (= :containid op) `(and (not (nil? ~lhs))
                             ;; any items where lhs.id = rhs? (want none)
                             (empty? (filter #(= % ~rhs) (map :id ~lhs))))
     (= :containitem op) `(and (not (nil? ~lhs))
                               ;; any items where lhs = rhs? (want none)
                               (empty? (filter #(= % ~rhs) ~lhs)))
     (= :like op) `(and (not (nil? ~lhs))
                        (not (.contains ~lhs ~rhs)))
     (= :ilike op) `(and (not (nil? ~lhs))
                         (not (.contains (lower-case ~lhs) (lower-case ~rhs))))
     :else `(and (not (nil? ~lhs))
                 (~not (~op ~lhs ~rhs))))))

(defn rewrite-empty-as-s-expression
  "Rewrites an 'empty' expression as a prefix s-expression."
  ([prop]
     `(and (not (nil? ~prop)) (empty? ~prop)))
  ([not prop]
     `(and (not (nil? ~prop)) (not (empty? ~prop)))))

(defn rewrite-null-as-s-expression
  "Rewrites a 'null' expression as a prefix s-expression."
  ([prop]
   `(nil? ~prop))
  ([not prop]
   `(not (nil? ~prop))))

;; This function uses *param-tags* for parameter names in the
;; resulting expression that may be dynamically bound
(defn rewrite-expr-as-ql-expression
  "Rewrites an expression (logical, comparison, etc.) as a ql-expression."
  ([expr]
     expr)
  ([not expr]
     (str not " " expr))
  ([lhs op rhs]
   (cond
     (= :contain op) (let [alias (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)]
                       (debug "The CONTAINS operator is deprecated.  Please use CONTAINSID going forward.")
                       (str "(exists (select " alias " from " lhs " " alias
                            " where " alias ".id = " rhs "))"))
     ;; containid checks to see if the value (an id) is one or more of the
     ;; ids of the items in the collection property
     (= :containid op) (let [alias (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)]
                         (str "(exists (select " alias " from " lhs " " alias
                              " where " alias ".id = " rhs "))"))
     ;; containitem checks to see if the value (the whole) is one or more of the
     ;; actual items in the collection property
     (= :containitem op) (let [alias (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)]
                           (str "(" rhs " MEMBER OF " lhs ")"))
     (= :like op) (str "(" lhs " LIKE " rhs ")") ;; already has %'s
     (= :ilike op) (str "(LOWER(" lhs ") LIKE LOWER(" rhs "))") ;; already has %'s
     :else (str "(" lhs " " op " " rhs ")")))
  ([not lhs op rhs]
   (cond
     (= :contain op) (let [alias (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)]
                       (debug "The CONTAINS operator is deprecated.  Please use CONTAINSID going forward.")
                       (str "(not exists (select " alias " from " lhs " " alias
                            " where " alias ".id = " rhs "))"))
     ;; containid checks to see if the value (an id) is one or more of the
     ;; ids of the items in the collection property
     (= :containid op) (let [alias (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)]
                         (str "(not exists (select " alias " from " lhs " " alias
                              " where " alias ".id = " rhs "))"))
     ;; containitem checks to see if the value (the whole) is one or more of the
     ;; actual items in the collection property
     (= :containitem op) (let [alias (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)]
                           (str "(" rhs " NOT MEMBER OF " lhs ")"))
     (= :like op) (str "(" lhs " NOT LIKE " rhs ")") ;; already has %'s
     (= :ilike op) (str "(LOWER(" lhs ") NOT LIKE LOWER(" rhs "))") ;; already has %'s
     :else (str "(" lhs " " not " " op " " rhs ")"))))

(defn rewrite-empty-as-ql-expression
  "Rewrites an 'empty' expression as a ql-expression."
  ([prop]
     (str "(" prop " IS EMPTY)"))
  ([not prop]
     (str "(" prop " IS NOT EMPTY")))

(defn rewrite-null-as-ql-expression
  "Rewrites a 'null' expression as a ql-expression."
  ([prop]
     (str "(" prop " IS NULL)"))
  ([not prop]
     (str "(" prop " IS NOT NULL)")))

(defn parse-date-str
  [s]
  (ctf/parse *date-format* s))

(def read-integer (comp int read-string))
(def read-long (comp long read-string))
(def read-float (comp float read-string))
(def read-double (comp double read-string))
(def read-boolean (fn [b] (read-string (.toLowerCase b))))
(def read-decimal (fn [s] (read-string (str s "M"))))
(def read-date-as-cal (fn [s] (.toCalendar (parse-date-str s)
                                          (java.util.Locale/getDefault))))
(def read-date-as-date (fn [s] (.toDate (parse-date-str s))))
(def read-date-as-epoch (fn [s] (.getMillis (parse-date-str s))))
(def read-enum (comp eval read-string)) ;; as long as the format is class/val

(defn emit-match-predicate
  "Takes a filter expression AST and generates a predicate function of a
map-like thing that tells whether or not it satisfies the filter."
  [ast]
  (let [m (gensym)]
    (->> ast (transform {:string identity
                         :likestring identity
                         :integer read-integer
                         :long read-long
                         :float read-float
                         :double read-double
                         :decimal read-decimal
                         :number read-string
                         :boolean read-boolean
                         :dateascal read-date-as-epoch
                         :dateasdate read-date-as-epoch
                         :enum symbol ;; going in the form to eval - need symbol
                         :eq (fn [_] #'=)
                         :ne (fn [_] #'not=)
                         :lt (fn [_] #'<)
                         :le (fn [_] #'<=)
                         :gt (fn [_] #'>)
                         :ge (fn [_] #'>=)
                         :contain (fn [_] :contain) ; tag for complete rewrite
                         :containid (fn [_] :containid) ; tag for complete rewrite
                         :containitem (fn [_] :containitem) ; tag for complete rewrite
                         :like (fn [_] :like) ; tag for complete rewrite
                         :ilike (fn [_] :ilike) ; tag for complete rewrite
                         :and (fn [_] #'and)
                         :or (fn [_] #'or)
                         :not (fn [_] #'not)
                         :property (fn [p] `(property-value ~m ~p))
                         :comparison rewrite-expr-as-s-expression
                         :emptycomparison rewrite-empty-as-s-expression
                         :nullcomparison rewrite-null-as-s-expression
                         :expression rewrite-expr-as-s-expression
                         :filter (fn [& args]
                                   (let [form `(fn [m#]
                                                 (let [~m (from-java m#)]
                                                   ~@args))]
                                     (debugf "Generated predicate function form: '%s'" form)
                                     form))})
         eval))) ;; the eval takes the form produced by the transform above
;; and evals it to create the actual function

(defn emit-query-info
  "Takes a filter expression AST and an entity-alias and generates
a predicate query information,
that is, a pair of JPA-style where-clause and map of parameter values.

If entity-alias is nil, no alias will be used in the generated
clause.

This dynamically binds *param-tags* to a new atom
wraping the value so any downstream functions
can pull values from the sequence without disturbing
the head of the sequence."
  [ast entity-alias]
  (binding [*param-tags* (atom @*param-tags*)]
    (let [param-map (atom {})
          convert-with (fn [f] (fn [s]
                                (let [k (let [a (first @*param-tags*)]
                                          (swap! *param-tags* rest)
                                          a)
                                      val (f s)]
                                  (swap! param-map #(assoc % k val))
                                  (str ":" k))))]
      (->> ast (transform {:string (fn [s] ;; add to the param map and
                                     ;; return the ql-format keyword
                                     (let [k (let [a (first @*param-tags*)]
                                               (swap! *param-tags* rest)
                                               a)]
                                       (swap! param-map #(assoc % k s))
                                       (str ":" k)))
                           ;; this one is handled specially to wrap in %'s
                           :likestring (convert-with
                                        (fn [s]
                                          (str "%"
                                               (str/replace
                                                (str/replace s "%" "\\%") "_" "\\_")
                                               "%")))
                           :integer (convert-with read-integer)
                           :long (convert-with read-long)
                           :float (convert-with read-float)
                           :double (convert-with read-double)
                           :decimal (convert-with read-decimal)
                           :number (convert-with read-string)
                           :dateascal (convert-with read-date-as-cal)
                           :dateasdate (convert-with read-date-as-date)
                           :enum (convert-with read-enum)
                           :boolean (fn [b] (.toUpperCase b)) ; no param val for bools
                           :eq (fn [_] "=")
                           :ne (fn [_] "<>")
                           :lt (fn [_] "<")
                           :le (fn [_] "<=")
                           :gt (fn [_] ">")
                           :ge (fn [_] ">=")
                           :contain (fn [_] :contain) ; tag for complete rewrite
                           :containid (fn [_] :containid) ; tag for complete rewrite
                           :containitem (fn [_] :containitem) ; tag for complete rewrite
                           :like (fn [_] :like) ; tag for complete rewrite
                           :ilike (fn [_] :ilike) ; tag for complete rewrite
                           :and (fn [_] "AND")
                           :or (fn [_] "OR")
                           :not (fn [_] "NOT")
                           :property (fn [p] (if (not (nil? entity-alias))
                                              (str entity-alias "." p)
                                              p)) ;; no alias, just drop in 'p'
                           :comparison rewrite-expr-as-ql-expression
                           :emptycomparison rewrite-empty-as-ql-expression
                           :nullcomparison rewrite-null-as-ql-expression
                           :expression rewrite-expr-as-ql-expression
                           :filter (fn [v]
                                     (let [qinfo {"where-clause" v
                                                  "param-map" @param-map}]
                                       (debugf "Generated query info: '%s'" qinfo)
                                       qinfo))})))))

(defn emit-sort-clause
  "Takes a sort expression AST and an entity-alias and generates
a JPA-style order-by clause.

If entity-alias is nil, no alias will be used in the generated
clause."
  [ast entity-alias]
  (->> ast (transform {:propertyclause (fn [& ps] ps)
                       :property (fn [p] (if (not (nil? entity-alias))
                                          (str entity-alias "." p)
                                          p)) ;; no alias, just drop in 'p'
                       :asc (fn [] "ASC")
                       :desc (fn [] "DESC")
                       :propertylist (fn [& xs]
                                       (apply str
                                              (interpose ","
                                                         (map #(apply str (interpose " " %)) xs))))
                       :sort identity}))) ;; it works out that proplist does all the work for this one

(defn emit-sort-comparator
  "Takes a sort expression AST and generates a comparator that implies
the order defined by that expression."
  [ast]
  (->> ast (transform {:propertyclause (fn [& ps] ps) ;; grab args as a vec
                       :property identity
                       :asc (fn [] #'+)  ;; turn into a modifier function
                       :desc (fn [] #'-) ;; switches the compare results
                       :propertylist (fn [& props] props) ;; used via apply...
                       :sort (fn [props]
                               (fn [a b] (compare-via-props a b props)))})
       (make-comparator))) ;; now take the compare function and use that to reify a comparator

(defn validate-parse
  "A function that either passed through an ast unchanged or
throws a big fit for java-callers to catch."
  [ast]
  (if (failure? ast)
    (throw (ParseFailure. (str "\n" (with-out-str (prn (get-failure ast))))))
    ast)) ; just pass on through

;;; And now, functions related to typical usage and java-callables

(defn make-match-predicate
  "Create a match predicate from a filter expression string.

This uses the default filter parser."
  [s]
  (debugf "Generating a match predicate from expression: '%s'" s)
  (->> s *filter-parser* validate-parse emit-match-predicate))

(def make-match-predicate-memo
  (lru make-match-predicate :lru/threshold memo-cache-size))

(defn make-query-info
  "Create query info from a filter expression string.

This uses the default filter parser and assumes the entity being
queried will be aliased to entity-alias for the where clause.

If entity-alias is nil, no alias will be used in the generated
clause."
  [s entity-alias]
  (debugf "Generating query info from expression: '%s'" s)
  (-> s *filter-parser* validate-parse (emit-query-info entity-alias)))

(def make-query-info-memo
  (lru make-query-info :lru/threshold memo-cache-size))

(defn make-sort-clause
  "Create sort clause from a sort expression string.

This uses the default sort parser and assumes the entity being
queried will be aliased to entity-alias for the where clause.

If entity-alias is nil, no alias will be used in the generated
clause."
  [s entity-alias]
  (debugf "Generating sort clause from expression: '%s'" s)
  (-> s *sort-parser* validate-parse (emit-sort-clause entity-alias)))

(def make-sort-clause-memo
  (lru make-sort-clause :lru/threshold memo-cache-size))

(defn make-sort-comparator
  "Create a sort comparator from a sort expression string.

This uses the default sort parser and expects the compared
objects' properties to be, themselves, Comparable."
  [s]
  (debugf "Generating a sort comparator from expression: '%s'" s)
  (->> s *sort-parser* validate-parse emit-sort-comparator))

(def make-sort-comparator-memo
  (lru make-sort-comparator :lru/threshold memo-cache-size))

(defn ^clojure.lang.IFn -makeMatchPredicate
  "A java callable wrapper around make-match-predicate-memo"
  [^String s]
  (infof "Generating match predicate from expression: '%s'" s)
  (make-match-predicate-memo s))

(defn ^java.util.Map -makeQueryInfo
  "A java callable wrapper around make-query-info-memo"
  ([^String s] (-makeQueryInfo s nil))
  ([^String s ^String entity-alias]
     (let [qinfo (make-query-info-memo s entity-alias)]
       (infof "Transformed filter expression '%s' into '%s'" s qinfo)
       qinfo)))

(defn ^String -makeSortClause
  "A java callable wrapper around make-sort-clause-memo"
  ([^String s] (-makeSortClause s nil))
  ([^String s ^String entity-alias]
     (let [clause (make-sort-clause-memo s entity-alias)]
       (infof "Transformed sort expression '%s' into '%s'" s clause)
       clause)))

(defn ^java.util.Comparator -makeSortComparator
  "A java callable wrapper around make-sort-comparator-memo
Note: The generated comparator will only work then the compared
objects' properties are, themselves, Comparable."
  [^String s]
  (infof "Generating sort comparator from expression: '%s'" s)
  (make-sort-comparator-memo s))
