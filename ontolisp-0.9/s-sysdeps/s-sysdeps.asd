;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: s-xml-rpc.asd,v 1.2 2004/06/17 19:43:11 rschlatte Exp $
;;;;
;;;; The S-SYSDEPS ASDF system definition
;;;;
;;;; Copyright (C) 2004-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-sysdeps
  :name "S-SYSDEPS"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "1"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "An abstraction layer over platform dependent functionality"
  :long-description "An abstraction layer over platform dependent functionality"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 #+clisp (:file "bivalent-streams" :depends-on ("package"))
                 (:file "sysdeps" :depends-on ("package" #+clisp "bivalent-streams"))))))

;;;; eof
