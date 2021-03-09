;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

(in-package :owl-syntaxes)

;;;
;;;;  owl-import.lisp
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
;;;   Purpose: Master import function of OWL 2 parser and OWL 2 syntax guesser 
;;; 

(defvar *owllink-mirror-mappings* nil)

(defvar *imported-ontologies* nil)

(defvar *ignore-import-p* nil)

(defvar *import-level* 0)

#+:racer-server
(defun get-syntax-from-document (uri &rest args)
  (declare (ignorable args))
  (racer::get-syntax-from-document 
   uri
   #+:aserve (getf args ':verbose)))

#-:racer-server
(defun get-syntax-from-document (uri &rest args)
  (declare (ignorable args))
  (with-input-from-url (stream uri)
    (loop 
      (let ((line (read-line stream nil nil)))
       (if line
           (let ((res 
                  (cond ((search "<rdf:RDF" line)
                         :owl-rdf)
                        ((search "<Ontology" line)
                         :owl-xml)
                        ((search "Ontology(" line)
                         :owl-functional))))
             (when res
               (return res)))
         (return :unknown))))))

(owlapi::defun1 owlapi-import-ontology (uri &rest args 
                                            &key syntax reasoner-name kb-name &allow-other-keys)
  
  (declare (ignorable args))

  (let* ((uri (if (symbolp uri)
                  (symbol-name uri)
                uri))
         (uri (or 
               (substitute-name uri *owllink-mirror-mappings*)
               uri))
         (reasoner *cur-reasoner*))
     
    (cond ((member (intern (owlapi::ensure-string uri)) *imported-ontologies*)

           #+:racer-server
           (reasoner-redundant-import-warning uri)
           #-:racer-server
           (owlapi-warning "Skipping redundant import: ~A" uri))

          (t

           (labels ((do-it ()

                      (when (or (not *ignore-import-p*)
                                (minusp *import-level*))

                        (unless (minusp *import-level*)
                          (owlapi::push-namespace-table reasoner))
             
                        (let* ((*import-level* (1+ *import-level*))
                               (*imported-ontologies* 
                                (cons (intern (owlapi::ensure-string uri))
                                      *imported-ontologies*))
                               (syntax 
                                (or syntax
                                    (get-syntax-from-document 
                                     uri))))

                          (let ((ontology (|OWLAPI-getAutoOntology| reasoner)))

                            (labels ((syntax-message (message)
                                       #+:racer-server
                                       (reasoner-syntax-message message uri)
                                       #-:racer-server
                                       (owlapi-warning message 0 uri)))

                              (unwind-protect
          
                                  (case syntax
                                    #+:racer-server
                                    ((:owl-rdf :rdf-xml :owl)

                                     (syntax-message "~%~V@TLooks like ontology ~A is in OWL RDF syntax~%")

                                     (apply #'racer::owl-read-document uri 
                                            :kb-name (or kb-name reasoner-name) ; wg. imports
                                            :allow-other-keys t
                                            args))

                                    #-:racer-server
                                    ((:owl-rdf :rdf-xml :owl)

                                     (owlapi-runtime-error "Cannot parse ontology ~A in OWL RDF syntax. Only OWL XML and OWL Functional are supported.~%" uri))

                                    ((:owl-xml :xml :owx)

                                     (syntax-message "~%~V@TLooks like ontology ~A is in OWL XML syntax~%")

                                     (apply #'owlapi-read-xml-ontology uri 
                                            :allow-other-keys t
                                            args))

                                    (t

                                     (syntax-message "~%~V@TAssuming ontology ~A is in OWL Functional syntax~%")

                                     (handler-case
                                         (apply #'owlapi-read-functional-ontology 
                                                uri
                                                :allow-other-keys t
                                                args)
                                       (error (error)
                                         (owlapi-runtime-error "Import of ontology ~A  failed - perhaps this ontology is neither in OWL RDF, OWL XML, nor OWL Functional syntax. Functional parser error was: ~A" uri error)))))
            
                                (let* ((ontology
                                        (if (owlapi::find-owl-ontology ontology nil)  
                                            ;; perhaps ontology was merged with another one 
                                            ;; (due to Import)
                                            ontology
                                          (|OWLAPI-getAutoOntology| reasoner))))

                                  (unless (eq ontology :void)
                                    (|OWLAPI-autoAddAxiomsTo| ontology reasoner)))))))
                                
                        (unless (minusp *import-level*)
                          (owlapi::pop-namespace-table reasoner))

                        (or kb-name reasoner-name))))

             (if (minusp *import-level*)
                 #+:racer-server
               (racer::without-duplicate-warnings (do-it))
                 #-:racer-server
                 (do-it)
               (do-it)))))))
