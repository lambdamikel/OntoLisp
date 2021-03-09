;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: s-xml-rpc.asd,v 1.2 2004/06/17 19:43:11 rschlatte Exp $
;;;;
;;;; The S-UTILS ASDF system definition
;;;;
;;;; Copyright (C) 2002-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-utils
  :name "S-UTILS"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "1"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "S-UTILS is collection of Common Lisp utilities"
  :long-description "S-UTILS is collection of Common Lisp utilities"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))))))

;;;; eof
