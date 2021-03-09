;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  nox-package.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2009 Nokia Corp. and/or its subsidiaries. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
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
;;;   Purpose: Definition for the package NOX
;;;


(in-package :cl-user)


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE NOX
;;;

(defpackage :nox
  (:nicknames :nokia-xml-cl
	      :wilbur-xml
              #+:allegro "NOX")
  (:use :common-lisp
	#+:mcl :ccl
	#+:excl :excl
	#+:sbcl :sb-sys)
  (:export #:xml-error                  ; from xml-util.lisp
	   #:define-constant
	   #:error-thing
	   #:syntax-error
	   #:pi-termination-problem
	   #:dtd-termination-problem
	   #:unexpected-end-tag
	   #:error-expectation
	   #:unknown-declaration
	   #:unknown-character-reference
	   #:malformed-url
	   #:feature-not-supported
	   #:missing-definition
	   #:error-definition-type
	   #:missing-entity-definition
	   #:missing-namespace-definition
	   #:xml-warning
	   #:*current-parser*
	   #:read-using
	   #:string-dict-get
	   #:string-dict-get-by-value
	   #:string-dict-add
	   #:string-dict-del
	   #:do-string-dict
	   #:make-file-url
	   #:make-http-url
	   #:parse-url
	   #:token
	   #:token-string
	   #:open-tag
	   #:close-tag
	   #:entity-declaration
	   #:entity-name
	   #:comment
	   #:char-content
	   #:tag-counterpart
	   #:tag-attribute
	   #:tag-attributes
	   #:tag-empty-p
	   #:tag-namespaces
	   #:start-element
	   #:end-element
	   #:char-content
	   #:proc-instruction
	   #:start-document
	   #:end-document
	   #:maybe-use-namespace
	   #:sax-consumer
	   #:sax-consumer-producer
	   #:sax-consumer-mode
	   #:sax-producer
	   #:sax-producer-consumer
	   #:sax-filter
	   #:find-first-producer
	   #:-whitespace-chars-
	   #:with-resource-from-pool
	   #:define-resource-pool
	   #:collapse-whitespace
	   #:*name-reader*              ; from xml-parser.lisp
	   #:xml-parser
	   #:get-entity
	   #:get-canonical-uri
	   #:parse
	   #:expand-name-with-namespace
	   #:parse-from-stream
	   #:parse-from-file
	   #:xml-formatter
	   #:replay
	   #:reverse-expand-name
	   #:tree-parser
	   #:string->keyword
	   #:parser-interpret-content
	   #:-rdf-uri-                  ; from rdf-constants.lisp
	   #:-rdfs-uri-
	   #:-xsd-uri-
	   #:rdf-uri
	   #:rdfs-uri
	   #:-rdf-attrs-
	   #:-rdf-attr-map-
	   #:-rdf-id-uri-
	   #:-rdf-resource-uri-
	   #:-rdf-about-uri-
	   #:-rdf-abouteach-uri-
	   #:-rdf-abouteachprefix-uri-
	   #:-rdf-bagid-uri-
	   #:-rdf-parsetype-uri-
	   #:-rdf-datatype-uri-
	   #:-rdf-nodeid-uri-
	   #:-xml-lang-attr-
	   #:-rdf-description-uri-
	   #:-rdf-type-uri-
	   #:-rdf-rdf-uri-
	   #:-rdf-li-uri-
	   #:-rdf-statement-uri-
	   #:-rdf-subject-uri-
	   #:-rdf-predicate-uri-
	   #:-rdf-object-uri-
	   #:-rdf-bag-uri-
	   #:-rdf-seq-uri-
	   #:-rdf-alt-uri-
	   #:-rdf-first-uri-
	   #:-rdf-rest-uri-
	   #:-rdf-nil-uri-
	   #:-rdfs-resource-uri-
	   #:-rdfs-class-uri-
	   #:-rdfs-subclassof-uri-
	   #:-rdfs-subpropertyof-uri-
	   #:-rdfs-seealso-uri-
	   #:-rdfs-isdefinedby-uri-
	   #:-rdfs-constraintresource-uri-
	   #:-rdfs-constraintproperty-uri-
	   #:-rdfs-range-uri-
	   #:-rdfs-domain-uri-
	   #:-rdfs-comment-uri-
	   #:-rdfs-label-uri-
	   #:-rdfs-literal-uri-
	   #:-rdfs-container-uri-

           #:tag-base
           #:tag-original-name
           #:parser-base
           #:parser-filepos
           #:*current-parser*
           #:wilbur-error))
