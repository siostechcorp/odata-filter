(ns com.sios.stc.odata.filter.protocols)

;; This is an interface instead of a protocol
;; because protocols don't (yet) support
;; the return type hints I want for Java interop.
;; (defprotocol IQueryInfo
;;   "Query information that can be used in JPA queries or SQL prepared statements"
;;   (^java.lang.String whereClause [this] "The where clause string to be appended to your own select clause")
;;   (^java.util.Map parameterMap [this] "The map of generated symbol string and its literal value from the original expression"))
(definterface IQueryInfo
  (^java.lang.String whereClause [])
  (^java.util.Map parameterMap []))
