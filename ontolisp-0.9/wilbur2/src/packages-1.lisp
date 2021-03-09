;;; -*- package: cl-user; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  packages.lisp
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
;;;   Purpose: This file contains the package definition for WILBUR.
;;;


(in-package :cl-user)


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE WILBUR
;;;

(defpackage :wilbur
  (:nicknames :w
	      :nox)			; so as not to have many packages anymore
  (:use :cl
	#+:mcl "CCL"
	#+:excl "excl"
	#+:excl "socket"
	#+:excl "mop"
	#+:sbcl "SB-SYS"
	#+:lispworks "MP")
  (:export #:*current-parser* 
	   #:*db* 
	   #:*name-reader* 
	   #:*nodes* 
	   #:-daml+oil-uri- 
	   #:-daml-first-uri- 
	   #:-daml-list-uri- 
	   #:-daml-nil-uri- 
	   #:-daml-rest-uri- 
	   #:-owl-first-uri- 
	   #:-owl-imports-uri- 
	   #:-owl-list-uri- 
	   #:-owl-nil-uri- 
	   #:-owl-rest-uri- 
	   #:-rdf-about-uri- 
	   #:-rdf-abouteach-uri- 
	   #:-rdf-abouteachprefix-uri- 
	   #:-rdf-alt-uri- 
	   #:-rdf-attr-map- 
	   #:-rdf-attrs- 
	   #:-rdf-bag-uri- 
	   #:-rdf-bagid-uri- 
	   #:-rdf-datatype-uri- 
	   #:-rdf-description-uri- 
	   #:-rdf-id-uri- 
	   #:-rdf-li-uri- 
	   #:-rdf-nodeid-uri- 
	   #:-rdf-object-uri- 
	   #:-rdf-parsetype-uri- 
	   #:-rdf-predicate-uri- 
	   #:-rdf-rdf-uri- 
	   #:-rdf-resource-uri- 
	   #:-rdf-seq-uri- 
	   #:-rdf-statement-uri- 
	   #:-rdf-subject-uri- 
	   #:-rdf-type-uri- 
	   #:-rdf-uri- 
	   #:-rdfs-class-uri- 
	   #:-rdfs-comment-uri- 
	   #:-rdfs-constraintproperty-uri- 
	   #:-rdfs-constraintresource-uri- 
	   #:-rdfs-container-uri- 
	   #:-rdfs-domain-uri- 
	   #:-rdfs-isdefinedby-uri- 
	   #:-rdfs-label-uri- 
	   #:-rdfs-literal-uri- 
	   #:-rdfs-range-uri- 
	   #:-rdfs-resource-uri- 
	   #:-rdfs-seealso-uri- 
	   #:-rdfs-subclassof-uri- 
	   #:-rdfs-subpropertyof-uri- 
	   #:-rdfs-uri- 
	   #:-whitespace-chars- 
	   #:-xml-lang-attr- 
	   #:about-and-id-both-present 
	   #:about-and-nodeid-both-present 
	   #:add-namespace 
	   #:add-triple 
	   #:add-value 
	   #:all-values 
	   #:attach-to-parent 
	   #:blank-node-db-mixin 
	   #:char-content 
	   #:char-content 
	   #:close-rdf-element 
	   #:close-tag 
	   #:collapse-whitespace 
	   #:collect-using-fsa 
	   #:comment 
	   #:container-required 
	   #:daml-cons 
	   #:daml-list 
	   #:daml-parser
	   #:date-cleanup-db-mixin
	   #:db 
	   #:db-add-triple 
	   #:db-blank-node-uri 
	   #:db-blank-node-uri-p 
	   #:db-clear 
	   #:db-clear-reasoner-cache 
	   #:db-del-source 
	   #:db-del-triple 
	   #:db-find-cbd 
	   #:db-find-source-desc
	   #:db-get-values
	   #:db-index-literals 
	   #:db-index-literals-p 
	   #:db-load 
	   #:db-load-using-source 
	   #:db-make-triple 
	   #:db-match-literals 
	   #:db-merge 
	   #:db-node-properties-partitioned 
	   #:db-node-type-p 
	   #:db-node-types 
	   #:db-query 
	   #:db-query-by-source 
	   #:db-reify 
	   #:db-resolve-blank-node-uri 
	   #:db-resolve-blank-node-uri 
	   #:db-sameas-clusters 
	   #:db-source-descs 
	   #:db-source-real-url 
	   #:db-sources 
	   #:db-startup-time 
	   #:db-supports-matching-p
	   #:db-transform-literal
	   #:db-triple-lock 
	   #:db-triples 
	   #:db-uri->blank-node 
	   #:deductive-closure-db-mixin 
	   #:defer-task 
	   #:define-readtable 
	   #:define-resource-pool 
	   #:del-namespace 
	   #:del-triple 
	   #:del-value 
	   #:dictionary 
	   #:dictionary-add-namespace 
	   #:dictionary-apropos-list 
	   #:dictionary-namespaces 
	   #:dictionary-node-class 
	   #:dictionary-nodes 
	   #:dictionary-remove-namespace 
	   #:dictionary-rename-namespace 
	   #:dictionary-unresolved-nodes 
	   #:do-string-dict 
	   #:dolist+ 
	   #:dsb 
	   #:dtd-termination-problem 
	   #:duplicate-namespace-prefix 
	   #:enable-literal-shorthand 
	   #:enable-node-shorthand 
	   #:end-document 
	   #:end-element 
	   #:entity-declaration 
	   #:entity-name 
	   #:error-definition-type 
	   #:error-expectation 
	   #:error-thing 
	   #:execute-deferred-task 
	   #:expand-name-with-namespace 
	   #:feature-not-supported 
	   #:file-url 
	   #:find-first-producer 
	   #:find-http-proxy 
	   #:find-long-name 
	   #:find-node 
	   #:find-short-name 
	   #:frame 
	   #:frames-related-p 
	   #:get-all-values 
	   #:get-canonical-uri 
	   #:get-entity 
	   #:get-header 
	   #:get-value 
	   #:http-body 
	   #:http-get 
	   #:http-head 
	   #:http-headers 
	   #:http-message 
	   #:http-status 
	   #:http-url 
	   #:http-version 
	   #:illegal-character-content 
	   #:index-uri 
	   #:index-uri-p 
	   #:indexed-db 
	   #:indexed-literal-db-mixin 
	   #:interned-literal 
	   #:interned-literal-db-mixin 
	   #:invert-path 
	   #:is-container-p 
	   #:iso8601-date-string 
	   #:literal
	   #:literal-datatype
	   #:literal-language 
	   #:literal-language-match-p 
	   #:literal-string
	   #:literal-transform-db-mixin
	   #:literal-value
	   #:load-db 
	   #:load-db-from-stream 
	   #:locked-db-mixin 
	   #:make-container 
	   #:make-file-url 
	   #:make-http-url 
	   #:make-lock 
	   #:make-triple-collection 
	   #:make-url 
	   #:malformed-url 
	   #:maybe-use-namespace 
	   #:missing-definition 
	   #:missing-entity-definition 
	   #:missing-namespace-definition 
	   #:namespaces 
	   #:node 
	   #:node-name-resolved-p 
	   #:node-uri 
	   #:open-http-stream 
	   #:open-tag 
	   #:out-of-sequence-index 
	   #:owl-uri 
	   #:own-slots 
	   #:parse 
	   #:parse-db-from-file 
	   #:parse-db-from-stream
	   #:parse-exif-date
	   #:parse-from-file
	   #:parse-from-stream 
	   #:parse-http-date 
	   #:parse-iso8601-date 
	   #:parse-url 
	   #:parse-using-parsetype 
	   #:parser-db 
	   #:parser-interpret-content 
	   #:parser-node 
	   #:parser-property 
	   #:path 
	   #:path-expression 
	   #:pi-termination-problem
	   #:prioritize
	   #:prioritize-list
	   #:proc-instruction 
	   #:query
	   #:quit-lisp-process
	   #:rdf-error 
	   #:rdf-parser 
	   #:rdf-syntax-normalizer 
	   #:rdf-uri 
	   #:rdfs-uri 
	   #:read-using 
	   #:reify 
	   #:relatedp 
	   #:replay 
	   #:reverse-expand-name 
	   #:sax-consumer 
	   #:sax-consumer-mode 
	   #:sax-consumer-producer 
	   #:sax-filter 
	   #:sax-producer 
	   #:sax-producer-consumer 
	   #:simple-external-process 
	   #:source-close-stream 
	   #:source-desc 
	   #:source-desc-load-time 
	   #:source-desc-loaded-from 
	   #:source-desc-url 
	   #:source-locator 
	   #:source-modification 
	   #:source-open-stream 
	   #:source-original-stream 
	   #:source-with-modification
	   #:split-list
	   #:start-document 
	   #:start-element 
	   #:string->keyword 
	   #:string-dict-add 
	   #:string-dict-del 
	   #:string-dict-get 
	   #:string-dict-get-by-value 
	   #:string-source 
	   #:syntax-error 
	   #:tag-attribute 
	   #:tag-attributes 
	   #:tag-counterpart 
	   #:tag-empty-p 
	   #:tag-namespaces 
	   #:task 
	   #:task-node 
	   #:task-parameter 
	   #:task-type 
	   #:token 
	   #:token-string 
	   #:tree-parser 
	   #:triple 
	   #:triple-collection-add 
	   #:triple-collection-triples 
	   #:triple-object
	   #:triple-predicate 
	   #:triple-sources 
	   #:triple-subject 
	   #:triple= 
	   #:unexpected-end-tag 
	   #:unknown-character-reference 
	   #:unknown-declaration 
	   #:unknown-parsetype 
	   #:url 
	   #:url-host 
	   #:url-path 
	   #:url-port 
	   #:url-string 
	   #:value 
	   #:walk-using-fsa 
	   #:with-db-lock 
	   #:with-http-response 
	   #:with-lock 
	   #:with-resource-from-pool 
	   #:with-temps 
	   #:without-closure 
	   #:xml-error 
	   #:xml-feature-not-supported 
	   #:xml-formatter 
	   #:xml-parser 
	   #:xml-warning 
	   #:xsd-uri

	   #:with-tags
	   #:format-with-tags
	   #:princ-with-tags
	   #:comma-separated
	   #:xhtml-preamble
	   #:xml-preamble
	   #:with-rdf-page
	   #:escape-json-string
	   #:escape-xml-string
	   #:serializer
	   #:serializer-stream
	   #:serializer-dump
	   #:single-subject-triples
	   #:rdf/xml-serializer))
