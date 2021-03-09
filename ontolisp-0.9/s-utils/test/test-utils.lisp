;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-base64.lisp,v 1.1.1.1 2004/06/09 09:02:41 scaekenberghe Exp $
;;;;
;;;; Unit and functional tests for S-UTILS
;;;;
;;;; Copyright (C) 2002-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-utils)

;; tokens

(assert (null (tokens "")))

(assert (equal (tokens "A") '("A")))

(assert (equal (tokens "A,B,C" :separators '(#\,)) '("A" "B" "C")))

(assert (equal (tokens "A B C D" :start 2 :end 5) '("B" "C")))

;; parse-integer-safely

(assert (null (parse-integer-safely nil)))

(assert (null (parse-integer-safely "")))

(assert (null (parse-integer-safely "NaN")))

(assert (eql 123 (parse-integer-safely "123")))

(assert (eql 123 (parse-integer-safely nil :default 123)))

(assert (eql 123 (parse-integer-safely "" :default 123)))

(assert (eql 123 (parse-integer-safely "NaN" :default 123)))

(assert (eql 123 (parse-integer-safely "111" :default 123 :start 3)))

(assert (eql 1 (parse-integer-safely "111" :default 123 :start 2)))

(assert (eql 123 (parse-integer-safely "123XXX" :end 3)))

;;;; eof
