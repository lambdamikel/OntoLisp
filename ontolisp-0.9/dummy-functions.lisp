;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

;;;
;;;;  dummy-functions.lisp
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
;;;   Purpose: Provides automatically generated dummy functions visualizing calls to 
;;;   an attached DL reasoner. These "bridge functions" must be implemented with real
;;;   API calls to a DL reasoner in order to bind a reasoner to OntoLisp
;;; 

(defparameter owlapi::*dummy-function-output* t)

(defun owlapi::reasoner-role-descendants (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-descendants~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-retrieve-individual-told-datatype-fillers (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-retrieve-individual-told-datatype-fillers~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-synonym-individuals (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-synonym-individuals~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-current-tbox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-current-tbox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-atomic-concept-synonyms (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-atomic-concept-synonyms~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-functional-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-functional-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-roles-disjoint-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-roles-disjoint-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-roles-equivalent-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-roles-equivalent-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-only-data-properties (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-only-data-properties~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-object-property-targets (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-object-property-targets~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-current-abox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-current-abox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-ensure-role (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-ensure-role~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-children (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-children~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-object-properties-of-target (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-object-properties-of-target~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-data-property-hierarchy2 (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-data-property-hierarchy2~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-disjoint-object-properties (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-disjoint-object-properties~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-children (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-children~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-data-property-synonyms (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-data-property-synonyms~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-types (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-types~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-only-object-properties (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-only-object-properties~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-dispose (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-dispose~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-inverse-roles (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-inverse-roles~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-ancestors (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-ancestors~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-tbox-classified-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-tbox-classified-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-tbox-sync (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-tbox-sync~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-object-property-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-object-property-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-symmetric-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-symmetric-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-asymmetric-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-asymmetric-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-role-range (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-role-range~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-sub-classes (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-sub-classes~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-delete-prefix-mappings (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-delete-prefix-mappings~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-satisfiable-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-satisfiable-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-instances (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-instances~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-instance-of-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-instance-of-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-clear-tbox-and-abox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-clear-tbox-and-abox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-synonym-individuals-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-synonym-individuals-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-data-properties-between (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-data-properties-between~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-data-property-sources (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-data-property-sources~{ ~S~})" args))
  nil)

(defun owlapi::|OWLAPI-enableSimplifiedProtocol| (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::|OWLAPI-enableSimplifiedProtocol|~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-individual-successors (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-individual-successors~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-ancestors (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-ancestors~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-is-instance-of (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-is-instance-of~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-role-domain (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-role-domain~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-taxonomy (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-taxonomy~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-individuals-related-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-individuals-related-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-data-properties (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-data-properties~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-classify (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-classify~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-entailed-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-entailed-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-transitive-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-transitive-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-reset-prefix-cache (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-reset-prefix-cache~{ ~S~})" args))
  nil)

(defun owlapi::|OWLAPI-disableSimplifiedProtocol| (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::|OWLAPI-disableSimplifiedProtocol|~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-prefixes (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-prefixes~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-irreflexive-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-irreflexive-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-data-property-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-data-property-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-data-property-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-data-property-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-subsumes-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-subsumes-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-ancestors (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-ancestors~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-disjoint-data-properties (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-disjoint-data-properties~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-antonym-individuals-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-antonym-individuals-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-object-properties-of-source (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-object-properties-of-source~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-object-properties (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-object-properties~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-symmetric-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-symmetric-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-remove-prefix (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-remove-prefix~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-clear-abox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-clear-abox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-instances (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-instances~{ ~S~})" args))
  nil)

(defun owlapi::|OWLAPI-usesSimplifiedProtocol| (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::|OWLAPI-usesSimplifiedProtocol|~{ ~S~})" args))
  nil)

(defun owlapi::|OWLAPI-storeImage| (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::|OWLAPI-storeImage|~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-retrieve-individual-fillers (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-retrieve-individual-fillers~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-is-used-as-annotation-property (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-is-used-as-annotation-property~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concepts-equivalent-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concepts-equivalent-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-parents (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-parents~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-class-hierarchy2 (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-class-hierarchy2~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-direct-entailed-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-direct-entailed-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-individual-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-individual-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-reflexive-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-reflexive-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-register-prefix (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-register-prefix~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-default-namespace-prefix (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-default-namespace-prefix~{ ~S~})" args))
  nil)

(defun owlapi::|OWLAPI-restoreImage| (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::|OWLAPI-restoreImage|~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-data-properties-of-literal (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-data-properties-of-literal~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-disjoint-classes (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-disjoint-classes~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-reflexive-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-reflexive-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-subsumes-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-subsumes-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-individual-has-data-filler-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-individual-has-data-filler-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-parents (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-parents~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-realize (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-realize~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-disjoint-individuals (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-disjoint-individuals~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-is-used-as-datatype-property (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-is-used-as-datatype-property~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-satisfiable-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-satisfiable-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-abox-realized-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-abox-realized-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-descendants (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-descendants~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-process-annotation (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-process-annotation~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-irreflexive-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-irreflexive-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concepts-disjoint-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concepts-disjoint-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-object-property-hierarchy2 (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-object-property-hierarchy2~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-antonym-individuals (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-antonym-individuals~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-transitive-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-transitive-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-tbox-language (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-tbox-language~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-concept-synonyms (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-concept-synonyms~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-object-properties-between (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-object-properties-between~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-parents (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-parents~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-abox-consistent-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-abox-consistent-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-children (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-children~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-set-current-abox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-set-current-abox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-object-property-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-object-property-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-inverse-functional-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-inverse-functional-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-inverse-functional-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-inverse-functional-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-individual-antonyms (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-individual-antonyms~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-individual-synonyms (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-individual-synonyms~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-object-property-sources (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-object-property-sources~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-inconsistent-concepts (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-inconsistent-concepts~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-sync (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-sync~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-equivalent-roles (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-equivalent-roles~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-abox-language (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-abox-language~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-abox-coherent-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-abox-coherent-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-new-tbox-and-abox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-new-tbox-and-abox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-set-current-tbox (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-set-current-tbox~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-role-functional-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-role-functional-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-super-classes (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-super-classes~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-data-properties-of-source (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-data-properties-of-source~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-types (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-types~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-equivalent-concepts (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-equivalent-concepts~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-get-data-property-targets (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-get-data-property-targets~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-concept-descendants (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-concept-descendants~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-role-asymmetric-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-role-asymmetric-p~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-get-individual-datatype-fillers (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-get-individual-datatype-fillers~{ ~S~})" args))
  nil)

(defun owlapi::reasoner-kb-is-consistent-p (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owlapi::reasoner-kb-is-consistent-p~{ ~S~})" args))
  nil)

(defun owl-syntaxes::owllink-object-property-synonyms (&rest args)
  (when owlapi::*dummy-function-output* (format t "~%~%DL reasoner dummy function called:
  (owl-syntaxes::owllink-object-property-synonyms~{ ~S~})" args))
  nil)

