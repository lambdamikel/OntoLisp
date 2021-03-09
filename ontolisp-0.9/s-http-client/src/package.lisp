;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is the S-HTTP-CLIENT package definition template
;;;;
;;;; Copyright (C) 2002-2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :s-http-client
  (:use common-lisp)
  (:export
   #:do-http-request
   #:http-client-state
   #:make-http-client-state
   #:*default-http-client-state*
   #:*http-client-agent*
   #:*http-client-accept*
   #:close-all-connections
   #:uri-encode-for-query
   #:uri-decode-for-query)
  (:documentation "A Basic HTTP Client"))

;;;; eof
