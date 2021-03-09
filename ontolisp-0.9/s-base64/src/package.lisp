;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is a Common Lisp implementation of Base64 encoding and decoding.
;;;;
;;;; Copyright (C) 2002-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage s-base64
  (:use common-lisp)
  (:export
   "DECODE-BASE64"
   "ENCODE-BASE64"
   "DECODE-BASE64-BYTES"
   "ENCODE-BASE64-BYTES")
  (:documentation "An implementation of standard Base64 encoding and decoding"))

;;;; eof
