;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is the S-SYSDEPS package definition
;;;;
;;;; Copyright (C) 2004-2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :s-sysdeps
  (:use common-lisp)
  (:export
   #:multiprocessing-capable-p
   #:current-process
   #:kill-process
   #:run-process
   #:all-processes
   #:start-standard-server
   #:open-socket-stream
   #:get-socket-stream-property
   #:make-process-lock
   #:with-process-lock)
  (:documentation "S-SYSDEPS is an abstraction layer over platform dependent functionality"))

;;;; eof
