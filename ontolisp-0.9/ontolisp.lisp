;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)
   
;;;
;;;;  ontolisp.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   OntoLisp (NOSA): A Semantic Web Framework for OWL 2 and OWLlink in Common Lisp
;;;
;;;   Copyright (c) 2007-2010 Michael Wessel and Racer Systems GmbH & Co. KG 
;;;   All Rights Reserved.
;;;
;;;   Contributor(s): Michael Wessel  (mailto:michael_wessel@gmx.de
;;;                                    mailto:wessel@racer-systems.com) 
;;;
;;;   This program is licensed under the terms of the GNU Lesser General Public License
;;;   as published by the Free Software Foundation, version 2.1 of the License. Note
;;;   however that a preamble attached below also applies to this program.
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Preamble to the Gnu Lesser General Public License
;;;
;;;   Copyright (c) 2000 Franz Incorporated, Berkeley, CA 94704
;;;
;;;   The concept of the GNU Lesser General Public License version 2.1 ("LGPL") has been
;;;   adopted to govern the use and distribution of above-mentioned application. However,
;;;   the LGPL uses terminology that is more appropriate for a program written in C than
;;;   one written in Lisp. Nevertheless, the LGPL can still be applied to a Lisp program
;;;   if certain clarifications are made. This document details those clarifications.
;;;   Accordingly, the license for the open-source Lisp applications consists of this
;;;   document plus the LGPL. Wherever there is a conflict between this document and the
;;;   LGPL, this document takes precedence over the LGPL.
;;;
;;;   A "Library" in Lisp is a collection of Lisp functions, data and foreign modules.
;;;   The form of the Library can be Lisp source code (for processing by an interpreter)
;;;   or object code (usually the result of compilation of source code or built with some
;;;   other mechanisms). Foreign modules are object code in a form that can be linked
;;;   into a Lisp executable. When we speak of functions we do so in the most general way
;;;   to include, in addition, methods and unnamed functions. Lisp "data" is also a
;;;   general term that includes the data structures resulting from defining Lisp classes.
;;;   A Lisp application may include the same set of Lisp objects as does a Library, but
;;;   this does not mean that the application is necessarily a "work based on the Library"
;;;   it contains.
;;;
;;;   The Library consists of everything in the distribution file set before any
;;;   modifications are made to the files. If any of the functions or classes in the
;;;   Library are redefined in other files, then those redefinitions ARE considered a
;;;   work based on the Library. If additional methods are added to generic functions in
;;;   the Library, those additional methods are NOT considered a work based on the
;;;   Library. If Library classes are subclassed, these subclasses are NOT considered a
;;;   work based on the Library. If the Library is modified to explicitly call other
;;;   functions that are neither part of Lisp itself nor an available add-on module to
;;;   Lisp, then the functions called by the modified Library ARE considered a work based
;;;   on the Library. The goal is to ensure that the Library will compile and run without
;;;   getting undefined function errors.
;;;
;;;   It is permitted to add proprietary source code to the Library, but it must be done
;;;   in a way such that the Library will still run without that proprietary code present.
;;;   Section 5 of the LGPL distinguishes between the case of a library being dynamically
;;;   linked at runtime and one being statically linked at build time. Section 5 of the
;;;   LGPL states that the former results in an executable that is a "work that uses the
;;;   Library." Section 5 of the LGPL states that the latter results in one that is a
;;;   "derivative of the Library", which is therefore covered by the LGPL. Since Lisp only
;;;   offers one choice, which is to link the Library into an executable at build time, we
;;;   declare that, for the purpose applying the LGPL to the Library, an executable that
;;;   results from linking a "work that uses the Library" with the Library is considered a
;;;   "work that uses the Library" and is therefore NOT covered by the LGPL.
;;;
;;;   Because of this declaration, section 6 of LGPL is not applicable to the Library.
;;;   However, in connection with each distribution of this executable, you must also
;;;   deliver, in accordance with the terms and conditions of the LGPL, the source code
;;;   of Library (or your derivative thereof) that is incorporated into this executable. 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Purpose: Loader for OntoLisp (NOSA) using ASDF
;;;

#+:allegro
(progn 
  (require :aserve)
  (require :inflate)
  (require :streama)
  (pushnew :aserve *features*))

(push :wilbur2 *features*)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (eq (readtable-case *readtable*) :preserve)
    (push :mlisp *features*))
  
  (unless *load-pathname*
    (break "\"ontolisp.lisp\" cannot be buffer-evaluated. Please use \"load\" or specify the \"logical-pathname-translation\" for host \"ontolisp\" by hand here:"))

  (let* ((p (probe-file *load-pathname*))
	 (d (pathname-directory p)))
    
    (if d
	(setf (logical-pathname-translations "ontolisp")
	  `(("**;*.*"
	     ,(merge-pathnames (make-pathname :name :wild :type :wild :version :wild
					      :directory '(:relative :wild-inferiors))
			       (make-pathname :name nil :type nil
					      :directory d
					      :defaults p)))))
      (break "File ~A not found." *load-pathname*))
    
    (setf (logical-pathname-translations "wilbur2")
      '(("**;*.*" "ontolisp:wilbur2;src;**;*.*")))

    (setf (logical-pathname-translations "test")
          '(("**;*.*" "ontolisp:test;**;*.*")))
    
    (format t "~%~%Base directory is: ~A" (translate-logical-pathname "ontolisp:"))))


(load "ontolisp:asdf;asdf.lisp")

(dolist (fn '("ontolisp:" 
              "ontolisp:asdf;"
              "ontolisp:puri;"
              "ontolisp:s-http-client;"
              "ontolisp:s-sysdeps;"
              "ontolisp:s-base64;"
              "ontolisp:s-utils;"
              "ontolisp:wilbur2;src;"))
  (push (translate-logical-pathname fn) asdf:*central-registry*))

(defun load-ontolisp (&optional force-p)
  (asdf:operate 'asdf:compile-op :ontolisp :force force-p)
  (asdf:operate 'asdf:load-op :ontolisp))

(load-ontolisp t)

;;; (owlapi::owlapi-test)
;;; (owl-syntaxes::owl-syntaxes-test)

