;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

(in-package :owl-syntaxes)

;;;
;;;;  import-export.lisp
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
;;;   Purpose: Top-level interface for OWL 2 import and export functionality. 
;;;            

(owlapi:defmethod1 write-owl-file ((ontology owlapi:ontology) (fn pathname) (syntax symbol)
                                    &rest args)
  
  (apply #'write-owl-file ontology (namestring fn) syntax args))
  
(owlapi:defmethod1 write-owl-file ((ontology owlapi:ontology) (fn string) (syntax symbol) 
                                   &key
                                   prefixes
                                   (p4-mode *p4-mode*)
                                   (comments *comments*) 
                                   &allow-other-keys)

  (with-open-file (stream fn
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    
    (let* ((*add-prefixes* prefixes)
           (*p4-mode* p4-mode)
           (*comments* comments)
           (axioms
            (render stream ontology syntax)))

      (when
          #+:racer-server
        *tbox-verbose*
        #-:racer-server t
        (format t "~%Rendered ~A axioms." axioms)
        (when (zerop axioms)
          (format t "~%Note that OWLAPI rendering only works if axiom objects were maintained! 
Please make sure that the ontology was loading using 
\"(owlapi-read-ontology <filename> :maintain-owlapi-axioms t)\".")))

      (if (zerop axioms)
          (list fn :warning-no-renderable-axioms-found)
        fn))))

;;;
;;;
;;;

(owlapi:owlapi-defun (|OWLAPI-writeOntologyFile|) (ontology fn &rest args)
  (#+:racer-server
   racer:without-duplicate-warnings
   #-:racer-server
   progn
   #+:racer-server
   (ts:check-if-unsafe)
   (let ((ont (owlapi:find-owl-ontology ontology)))
     (apply #'write-owl-file ont fn :owl-rdf args))))

(owlapi:owlapi-defun (|OWLAPI-writeFunctionalOntologyFile|) (ontology fn &rest args)
  (#+:racer-server
   racer:without-duplicate-warnings
   #-:racer-server
   progn
   #+:racer-server
   (ts:check-if-unsafe)
    (let ((ont (owlapi:find-owl-ontology ontology)))
      (apply #'write-owl-file ont fn :owl-functional args))))

(owlapi:owlapi-defun (|OWLAPI-writeXMLOntologyFile|) (ontology fn &rest args)
  (#+:racer-server
   racer::without-duplicate-warnings
   #-:racer-server
   progn
   #+:racer-server
   (ts::check-if-unsafe)
   (let ((ont (owlapi:find-owl-ontology ontology)))
      (apply #'write-owl-file ont fn :owl-xml args))))

;;;
;;; Export Master
;;;

(owlapi:owlapi-defun (|OWLAPI-saveOntology|) (ontology fn &rest args
                                              &key 
                                              reasoner
                                              (syntax :owl-rdf) 
                                              prefixes
                                              (p4-mode *p4-mode*)
                                              (comments *comments*) 
                                              &allow-other-keys)

  (declare (ignorable prefixes p4-mode comments))
  
  (#+:racer-server
   racer::without-duplicate-warnings
   #-:racer-server
   progn
    (with-reasoner (reasoner)
      (ecase syntax
        ((:owl-rdf :rdf-xml :owl)
         (apply #'|OWLAPI-writeOntologyFile| ontology fn args))
        ((:owl-functional :ofn :owf :funct :functional) 
         (apply #'|OWLAPI-writeFunctionalOntologyFile| ontology fn args))
        ((:owl-xml :xml :owx)
         (apply #'|OWLAPI-writeXMLOntologyFile| ontology fn args))))))

;;;
;;; Import Master 
;;;

(owlapi:owlapi-defun (|OWLAPI-readOntology|) (url &rest args
                                         &key 
                                         (ignore-import *ignore-import-p*)
                                         &allow-other-keys)

  (let ((*imported-ontologies* nil)
        (*import-level* -1) ; richtig! 
        (*ignore-import-p* ignore-import))

    (if (not (owlapi:is-url-p url))
        (apply #'owlapi-import-ontology (make-url-from-filename url) 
	       :allow-other-keys t
	       args)
      (apply #'owlapi-import-ontology url 
	     :allow-other-keys t
	     args))))

