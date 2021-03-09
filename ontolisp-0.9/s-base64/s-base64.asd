;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: s-xml-rpc.asd,v 1.2 2004/06/17 19:43:11 rschlatte Exp $
;;;;
;;;; The S-BASE64 ASDF system definition
;;;;
;;;; Copyright (C) 2002-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-base64
  :name "S-BASE64"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "2"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "Common Lisp Base64 Package"
  :long-description "S-BASE64 is a Common Lisp implementation of Base64 Encoding/Decoding"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "base64" :depends-on ("package"))))))

;;;; eof
