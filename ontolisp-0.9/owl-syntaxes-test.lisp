;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

(in-package :owl-syntaxes)

;;;
;;;;  owl-syntaxes-test.lisp
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
;;;   Purpose: Demo and tests for the OWL 2 and OWLlink parsing and rendering functionality. 
;;; 

(defun temp-directory () "ontolisp:test;temp;")

(defun full-reset ()
  (owlapi::owlapi-init))

(defun read-owlapi-ontology (file &rest args)
  (apply #'|OWLAPI-readOntology| 
         (format nil "file://~A"
                 (namestring 
                  (translate-logical-pathname
                   (format nil "ontolisp-test:~A" file))))
         args))


(defun read-owlapi-ontology-from-temp (file &rest args)
  (apply #'|OWLAPI-readOntology| 
         (format nil "file://~A"
                 (namestring 
                  (translate-logical-pathname
                   (format nil "ontolisp-test:temp;~A" file))))
         args))

(defun write-owlapi-ontology-file (file &rest args)
  (apply #'|OWLAPI-saveOntology| 
         (first (|OWLAPI-getOntologies|)) 
         (format nil "~A~A" 
                 (temp-directory)
                 file)
         args))

;;;
;;;
;;;					 
    
(defun owl-syntaxes-test ()
  (let ((*count* 0)
        (*demo* 0)
        (*package* (find-package :owl-syntaxes)))

    (progn

      (incf *demo*)

      ;; Demo 1: An ontology OWL 2 functional ontology can be parsed
      ;; from a file or http URL (this is why s-http-client is used
      ;; for in OntoLisp).  Note that an OWLAPI reasoner container is
      ;; created for the whole request and named after the URL (unless
      ;; specified via the :reasoner keyword), and for each imported
      ;; ontology, a corresponding ontology container.  I.e., the
      ;; reasoner
      ;; |file:///home/mi.wessel/nosa/test/people-pets-newest.funct|
      ;; contains the ontology container
      ;; |http://cohse.semanticweb.org/ontologies/people| (that's the
      ;; ontology URL / name specified in the file). In principle, the
      ;; OWLAPI axioms can be mainted / kept, or not. Axiom objects
      ;; may require a lot of memory for larger ontologies. Note that,
      ;; it maintain-owlapi-axioms nil is specified, no axioms will be
      ;; created, and every OWLAPI constructor call will directly be
      ;; comminucated to the DL reasoner. However, axioms can then
      ;; neither be unloaded incrementally nor ontologies rendered
      ;; (see below). Also note that |OWLAPI-readOntology| has some
      ;; important keyword arguments regarding the handling of imports
      ;; of ontologies (please consult the function
      ;; "owlapi-process-functional-ontology" for the arguments):

      (demo (full-reset))

      (demo (read-owlapi-ontology "people-pets-newest.funct" :maintain-owlapi-axioms nil))

      (demo (|OWLAPI-getOntologies|))

      (demo (|OWLAPI-getReasoners|)))

    (progn 

      (incf *demo*)
      
      ;; Demo 2: The OWL 2 parser understands OWL 2 functional syntax
      ;; and OWL 2 XML syntax.  OWL 2 RDF will be added in a later
      ;; version. If the told information is kept, i.e., the ontology
      ;; axioms are maintained, then it is possible to use OntoLisp as
      ;; a syntax converter, and render ontologies in all major OWL 2
      ;; syntaxes as follow.  Note that these generated files are
      ;; under "ontolisp:test;temp;":

      (demo (full-reset))

      (demo (read-owlapi-ontology "people-pets-newest.funct" :maintain-owlapi-axioms t))

      (demo (owlapi-get-axioms))

      (demo (write-owlapi-ontology-file "people-pets-newest.owf" :syntax :owl-functional))

      (demo (write-owlapi-ontology-file "people-pets-newest.owx" :syntax :owl-xml))

      (demo (write-owlapi-ontology-file "people-pets-newest.owl" :syntax :owl-rdf)))

    (progn 

      (incf *demo*)

      ;; Demo 3: Here we just demonstrate that the just produced
      ;; converted ontologies can be parsed:
      
      (demo (full-reset))

      (demo (read-owlapi-ontology-from-temp "people-pets-newest.owf" :maintain-owlapi-axioms t))

      (demo (owlapi-get-axioms))

      (demo (full-reset))

      (demo (read-owlapi-ontology-from-temp "people-pets-newest.owx" :maintain-owlapi-axioms t))

      (demo (owlapi-get-axioms))

      (demo (full-reset))
      
      #+:ignore 
      (demo (read-owlapi-ontology-from-temp "people-pets-newest.owl" :maintain-owlapi-axioms t))

      (demo (owlapi-get-axioms)))

    (progn 

      (incf *demo*)

      ;; Demo 4: Some more simple parsing tests: 
      
      (dolist (fn '("owl-primer.funct"
                    "owl-primer-mod.ofn"
                    "people-pets-newest.funct"
                    "owl-primer-mod.owx"))

        (demo (full-reset))

        (demo (read-owlapi-ontology fn :maintain-owlapi-axioms t))
    
        (demo (owlapi-get-axioms))))

    (progn

      ;; Demo 5: OWLlink demo. We are processing a simple OWLlink XML
      ;; request. Note that this gives no reasonable ResponseMessage,
      ;; since the corresponding dummy functions do not really work.
      ;; Still, the overall framework is working, e.g., an ontology
      ;; with axioms for the corresponding CreateKB and Tell
      ;; statements have been created in the OWLAPI:

      (incf *demo*)
      
      (demo (full-reset))

      (demo (owllink-read-file1 "ontolisp-test:owllink-test-request.xml" :maintain-owlapi-axioms t))

      (demo (owlapi-get-axioms)))

    (progn 

      ;; Demo 6: OntoLisp contains an OWLlink converter. There are
      ;; currently three bindings for OWLlink defined (OWLlink XML,
      ;; OWLlink Functional, and OWLlink S-Expressions). This
      ;; convertes owllink-test-request.xml in OWLlink XML syntax into
      ;; OWLlink functional and OWLlink S-Expression syntax:
    
      (incf *demo*)
  
      (setf owlapi::*dummy-function-output* nil)

      (demo (full-reset))
      
      (demo (convert-all (translate-logical-pathname "ontolisp-test:"))))

    (progn 

      ;; Demo 7: Demonstrate that the generated / converted OWLlink
      ;; messages can in principle be processed. Note that the
      ;; Response messages contain errors, of course (since only the
      ;; dummy functions are called), but OWLAPI axiom parsing and
      ;; handling works:

      (incf *demo*)
      
      (setf owlapi::*dummy-function-output* nil)

      (demo (full-reset))

      (demo (process-all (translate-logical-pathname "ontolisp-test:"))))

    (progn 
      
      (setf owlapi::*dummy-function-output* t))))
      

