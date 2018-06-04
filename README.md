# odata-filter

*A library for converting OData-style filter expressions
into JPQL/HQL style 'where' clauses and into in-memory predicate/comparator
functions.*

## Features

odata-filter allows OData-style filter expressions, such as "name eq 'joe'", into
JPQL (HQL) where clauses like "alias.name = 'joe'".  It can also convert
filter expressions into predicate/comparator functions to be used on
in-memory data.

+ Convert filter expressions into "where" clauses, along with literal values
  ready to be used in prepared queries/statements
+ Convert filter expressions into predicate/comparator functions that can
  be used on in-memory clojure and java data (collections, objects, etc.)
+ More to come...

## Quickstart


Add the following to your leiningen project file (once binaries are published):

	[com.sios.stc/odata-filter "1.0.0-SNAPSHOT"]

Require the namespace:

	(ns myns (:require [com.sios.stc.odata.filter.core :as od]))


## License

Distributed under the terms of the MIT License.

