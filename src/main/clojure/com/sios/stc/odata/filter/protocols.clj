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
