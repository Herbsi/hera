;;;; package.lisp

(defpackage #:hera
  (:use #:cl #:iter)
  #+sb-package-locks
  (:lock t)
  (:import-from #:new-let
                #:fn)
  (:shadowing-import-from #:new-let
                          #:let)
  (:export))
