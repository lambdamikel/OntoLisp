;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is the S-UTILS package definition
;;;;
;;;; Copyright (C) 2004-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :s-utils
  (:use common-lisp)
  (:export
   #:make-subdirectory
   #:pathname-parent
   #:copy-stream
   #:tokens
   #:format-universal-time
   #:format-duration
   #:format-iso-gmt-time
   #:parse-integer-safely
   #:+us-day-names+
   #:+us-month-names+
   #:+us-time-format+
   #:+us-date-format+
   #:+en-duration-unit-names+)
  (:documentation "S-UTILS is collection of Common Lisp utilities"))

;;;; eof
