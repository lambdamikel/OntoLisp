;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-base64.lisp,v 1.1.1.1 2004/06/09 09:02:41 scaekenberghe Exp $
;;;;
;;;; Unit and functional tests for S-HTTP-CLIENT
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-client)

(assert (do-http-request "http://localhost"))

(assert (do-http-request "https://secure.beta9.be"))

;;;; eof
