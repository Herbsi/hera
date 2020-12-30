;;;; package.lisp

(defpackage #:hera
  (:use #:cl #:iter)
  #+sb-package-locks
  (:lock t)
  (:import-from #:alexandria
                #:when-let
                #:compose
                #:mappend
                #:with-gensyms)
  (:import-from #:defclass-std
                #:defclass/std)
  (:import-from #:gmap
                #:gmap)
  (:import-from #:new-let
                #:fn)
  (:shadowing-import-from #:new-let
                          #:let)
  (:export
   #:hera))
