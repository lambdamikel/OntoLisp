;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

(in-package :owlapi)

;;;
;;;;  owlapi-synonyms.lisp
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
;;;   Purpose: Provide synonyms for |OWLAPI-...| functions which are more Lisp-friendly. 
;;; 

;;;
;;;----------------------------------------------
;;;   Automatically Generated OWLAPI Synonyms   
;;;          Version: 2.0, Build: 2010-06-18 
;;;          Date: June 21 2010, 13:31  
;;;----------------------------------------------
;;;

(progn
  (owlapi-defun (owlapi-write-xml-ontology-file)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-writeXMLOntologyFile| common-lisp-user::args))
  (owlapi-defun (owlapi-write-ontology-file)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-writeOntologyFile| common-lisp-user::args))
  (owlapi-defun (owlapi-write-functional-ontology-file)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-writeFunctionalOntologyFile|
           common-lisp-user::args))
  (owlapi-defun (owlapi-uses-simplified-protocol)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-usesSimplifiedProtocol| common-lisp-user::args))
  (owlapi-defun (owlapi-uses-incremental-updates)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-usesIncrementalUpdates| common-lisp-user::args))
  (owlapi-defun (owlapi-unload-ontology) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-unloadOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-unload-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-unloadOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-unload-axioms) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-unloadAxioms| common-lisp-user::args))
  (owlapi-defun (owlapi-unload-axiom) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-unloadAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-store-image) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-storeImage| common-lisp-user::args))
  (owlapi-defun (owlapi-set-return-policy)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-setReturnPolicy| common-lisp-user::args))
  (owlapi-defun (owlapi-set-current-reasoner)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-setCurrentReasoner| common-lisp-user::args))
  (owlapi-defun (owlapi-set-axiom-counter)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-setAxiomCounter| common-lisp-user::args))
  (owlapi-defun (owlapi-set-auto-declare-data-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-setAutoDeclareDataProperties|
           common-lisp-user::args))
  (owlapi-defun (owlapi-save-ontology) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-saveOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-restore-image) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-restoreImage| common-lisp-user::args))
  (owlapi-defun (owlapi-reset-axiom-counter)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-resetAxiomCounter| common-lisp-user::args))
  (owlapi-defun (owlapi-remove-prefix) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-removePrefix| common-lisp-user::args))
  (owlapi-defun (owlapi-remove-axioms) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-removeAxioms| common-lisp-user::args))
  (owlapi-defun (owlapi-remove-axiom) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-removeAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-reload-loaded-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-reloadLoadedOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-register-referenced-entities)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-registerReferencedEntities|
           common-lisp-user::args))
  (owlapi-defun (owlapi-register-object) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-registerObject| common-lisp-user::args))
  (owlapi-defun (owlapi-register-last-answer)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-registerLastAnswer| common-lisp-user::args))
  (owlapi-defun (owlapi-register-declared-entities)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-registerDeclaredEntities| common-lisp-user::args))
  (owlapi-defun (owlapi-realize) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-realize| common-lisp-user::args))
  (owlapi-defun (owlapi-read-xml-ontology-file)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-readXMLOntologyFile| common-lisp-user::args))
  (owlapi-defun (owlapi-read-xml-ontology-document)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-readXMLOntologyDocument| common-lisp-user::args))
  (owlapi-defun (owlapi-read-ontology) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-readOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-read-functional-ontology-file)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-readFunctionalOntologyFile|
           common-lisp-user::args))
  (owlapi-defun (owlapi-read-functional-ontology-document)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-readFunctionalOntologyDocument|
           common-lisp-user::args))
  (owlapi-defun (owlapi-parse-native) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-parseNative| common-lisp-user::args))
  (owlapi-defun (owlapi-parse) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-parse| common-lisp-user::args))
  (owlapi-defun (owlapi-next-axiom-use-id)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-nextAxiomUseID| common-lisp-user::args))
  (owlapi-defun (owlapi-new-reasoner) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-newReasoner| common-lisp-user::args))
  (owlapi-defun (owlapi-new-ontology) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-newOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-merge-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-mergeOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-manually-apply-changes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-manuallyApplyChanges| common-lisp-user::args))
  (owlapi-defun (owlapi-load-ontology) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-loadOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-load-ontologies) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-loadOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-load-axioms) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-loadAxioms| common-lisp-user::args))
  (owlapi-defun (owlapi-load-axiom) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-loadAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-keep-annotations)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-keepAnnotations| common-lisp-user::args))
  (owlapi-defun (owlapi-is-transitive) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isTransitive| common-lisp-user::args))
  (owlapi-defun (owlapi-is-symmetric) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isSymmetric| common-lisp-user::args))
  (owlapi-defun (owlapi-is-sub-class-of) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isSubClassOf| common-lisp-user::args))
  (owlapi-defun (owlapi-is-satisfiable) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isSatisfiable| common-lisp-user::args))
  (owlapi-defun (owlapi-is-same-individual)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isSameIndividual| common-lisp-user::args))
  (owlapi-defun (owlapi-is-reflexive) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isReflexive| common-lisp-user::args))
  (owlapi-defun (owlapi-is-realised) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isRealised| common-lisp-user::args))
  (owlapi-defun (owlapi-is-irreflexive) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isIrreflexive| common-lisp-user::args))
  (owlapi-defun (owlapi-is-inverse-functional)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isInverseFunctional| common-lisp-user::args))
  (owlapi-defun (owlapi-is-functional) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isFunctional| common-lisp-user::args))
  (owlapi-defun (owlapi-is-equivalent-class)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isEquivalentClass| common-lisp-user::args))
  (owlapi-defun (owlapi-is-different-individual)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isDifferentIndividual| common-lisp-user::args))
  (owlapi-defun (owlapi-is-defined-object-property)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isDefinedObjectProperty| common-lisp-user::args))
  (owlapi-defun (owlapi-is-defined-individual)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isDefinedIndividual| common-lisp-user::args))
  (owlapi-defun (owlapi-is-defined-data-property)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isDefinedDataProperty| common-lisp-user::args))
  (owlapi-defun (owlapi-is-defined-class)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isDefinedClass| common-lisp-user::args))
  (owlapi-defun (owlapi-is-consistent) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isConsistent| common-lisp-user::args))
  (owlapi-defun (owlapi-is-classified) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isClassified| common-lisp-user::args))
  (owlapi-defun (owlapi-is-class) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isClass| common-lisp-user::args))
  (owlapi-defun (owlapi-is-asymmetric) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-isAsymmetric| common-lisp-user::args))
  (owlapi-defun (owlapi-init) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-init| common-lisp-user::args))
  (owlapi-defun (owlapi-ignore-declarations)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-ignoreDeclarations| common-lisp-user::args))
  (owlapi-defun (owlapi-ignore-annotations)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-ignoreAnnotations| common-lisp-user::args))
  (owlapi-defun (owlapi-has-type) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-hasType| common-lisp-user::args))
  (owlapi-defun (owlapi-has-object-property-relationship)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-hasObjectPropertyRelationship|
           common-lisp-user::args))
  (owlapi-defun (owlapi-has-data-property-relationship)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-hasDataPropertyRelationship|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-types) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getTypes| common-lisp-user::args))
  (owlapi-defun (owlapi-get-super-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getSuperProperties| common-lisp-user::args))
  (owlapi-defun (owlapi-get-super-classes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getSuperClasses| common-lisp-user::args))
  (owlapi-defun (owlapi-get-sub-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getSubProperties| common-lisp-user::args))
  (owlapi-defun (owlapi-get-sub-classes) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getSubClasses| common-lisp-user::args))
  (owlapi-defun (owlapi-get-same-individuals)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getSameIndividuals| common-lisp-user::args))
  (owlapi-defun (owlapi-get-related-values)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getRelatedValues| common-lisp-user::args))
  (owlapi-defun (owlapi-get-related-individuals)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getRelatedIndividuals| common-lisp-user::args))
  (owlapi-defun (owlapi-get-reasoners) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getReasoners| common-lisp-user::args))
  (owlapi-defun (owlapi-get-ranges) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getRanges| common-lisp-user::args))
  (owlapi-defun (owlapi-get-prefixes) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getPrefixes| common-lisp-user::args))
  (owlapi-defun (owlapi-get-ontologies) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-get-object-property-relationships)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getObjectPropertyRelationships|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-transitive-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLTransitiveObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-symmetric-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLSymmetricObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-sub-class-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLSubClassAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-sub-annotation-property-of-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLSubAnnotationPropertyOfAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-sub-annotation-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLSubAnnotationPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-same-individuals-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLSameIndividualsAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-reflexive-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLReflexiveObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-really-implicit-declaration-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLReallyImplicitDeclarationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-prefix-declaration-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLPrefixDeclarationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-ontology-version-declaration-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLOntologyVersionDeclarationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-ontology-annotation-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLOntologyAnnotationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-object-sub-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLObjectSubPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-object-property-range-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLObjectPropertyRangeAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-object-property-domain-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLObjectPropertyDomainAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-object-property-chain-sub-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-object-property-assertion-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLObjectPropertyAssertionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-negative-object-property-assertion-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-negative-data-property-assertion-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-irreflexive-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-inverse-object-properties-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLInverseObjectPropertiesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-inverse-functional-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-imports-declaration-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLImportsDeclarationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-implicit-declaration-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLImplicitDeclarationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-has-key-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLHasKeyAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-functional-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLFunctionalObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-functional-data-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLFunctionalDataPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-equivalent-object-properties-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLEquivalentObjectPropertiesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-equivalent-data-properties-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLEquivalentDataPropertiesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-equivalent-classes-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLEquivalentClassesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-entity-annotation-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLEntityAnnotationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-disjoint-union-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDisjointUnionAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-disjoint-object-properties-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDisjointObjectPropertiesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-disjoint-data-properties-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDisjointDataPropertiesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-disjoint-classes-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDisjointClassesAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-different-individuals-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDifferentIndividualsAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-declaration-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDeclarationAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-datatype-definition-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDatatypeDefinitionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-data-sub-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDataSubPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-data-property-range-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDataPropertyRangeAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-data-property-domain-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDataPropertyDomainAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-data-property-assertion-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLDataPropertyAssertionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-class-assertion-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLClassAssertionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-axiom-annotation-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLAxiomAnnotationAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-asymmetric-object-property-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLAsymmetricObjectPropertyAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-annotation-property-range-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLAnnotationPropertyRangeAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-annotation-property-domain-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLAnnotationPropertyDomainAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-owl-annotation-assertion-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getOWLAnnotationAssertionAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-loaded-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getLoadedOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-get-inverse-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getInverseProperties| common-lisp-user::args))
  (owlapi-defun (owlapi-get-individuals) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getIndividuals| common-lisp-user::args))
  (owlapi-defun (owlapi-get-inconsistent-classes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getInconsistentClasses| common-lisp-user::args))
  (owlapi-defun (owlapi-get-equivalent-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getEquivalentProperties| common-lisp-user::args))
  (owlapi-defun (owlapi-get-equivalent-classes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getEquivalentClasses| common-lisp-user::args))
  (owlapi-defun (owlapi-get-domains) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getDomains| common-lisp-user::args))
  (owlapi-defun (owlapi-get-different-individuals)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getDifferentIndividuals| common-lisp-user::args))
  (owlapi-defun (owlapi-get-descendant-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getDescendantProperties| common-lisp-user::args))
  (owlapi-defun (owlapi-get-descendant-classes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getDescendantClasses| common-lisp-user::args))
  (owlapi-defun (owlapi-get-data-property-relationships)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getDataPropertyRelationships|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-current-reasoner)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getCurrentReasoner| common-lisp-user::args))
  (owlapi-defun (owlapi-get-changes) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getChanges| common-lisp-user::args))
  (owlapi-defun (owlapi-get-axioms-per-ontology)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAxiomsPerOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-get-axioms-of-type-in)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAxiomsOfTypeIn| common-lisp-user::args))
  (owlapi-defun (owlapi-get-axioms-of-type)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAxiomsOfType| common-lisp-user::args))
  (owlapi-defun (owlapi-get-axioms-in) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAxiomsIn| common-lisp-user::args))
  (owlapi-defun (owlapi-get-axioms) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAxioms| common-lisp-user::args))
  (owlapi-defun (owlapi-get-axiom-counter)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAxiomCounter| common-lisp-user::args))
  (owlapi-defun (owlapi-get-auto-ontology)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAutoOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-get-auto-declare-data-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAutoDeclareDataProperties|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-annotation-axioms-for-axiom)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAnnotationAxiomsForAxiom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-get-ancestor-properties)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAncestorProperties| common-lisp-user::args))
  (owlapi-defun (owlapi-get-ancestor-classes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAncestorClasses| common-lisp-user::args))
  (owlapi-defun (owlapi-get-all-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-getAllOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-find-object-from-id)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-findObjectFromID| common-lisp-user::args))
  (owlapi-defun (owlapi-find-id-from-object)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-findIDFromObject| common-lisp-user::args))
  (owlapi-defun (owlapi-export-reasoner) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-exportReasoner| common-lisp-user::args))
  (owlapi-defun (owlapi-export-ontology) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-exportOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-enable-transient-axiom-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-enableTransientAxiomMode| common-lisp-user::args))
  (owlapi-defun (owlapi-enable-simplified-protocol)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-enableSimplifiedProtocol| common-lisp-user::args))
  (owlapi-defun (owlapi-enable-memory-saving-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-enableMemorySavingMode| common-lisp-user::args))
  (owlapi-defun (owlapi-enable-lookup-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-enableLookupMode| common-lisp-user::args))
  (owlapi-defun (owlapi-enable-incremental-updates)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-enableIncrementalUpdates| common-lisp-user::args))
  (owlapi-defun (owlapi-dont-register-referenced-entities)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-dontRegisterReferencedEntities|
           common-lisp-user::args))
  (owlapi-defun (owlapi-dont-register-declared-entities)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-dontRegisterDeclaredEntities|
           common-lisp-user::args))
  (owlapi-defun (owlapi-dispose-reasoner)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disposeReasoner| common-lisp-user::args))
  (owlapi-defun (owlapi-dispose-ontology)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disposeOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-dispose-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disposeOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-dispose-axioms) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disposeAxioms| common-lisp-user::args))
  (owlapi-defun (owlapi-dispose-axiom) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disposeAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-dispose) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-dispose| common-lisp-user::args))
  (owlapi-defun (owlapi-disable-transient-axiom-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disableTransientAxiomMode|
           common-lisp-user::args))
  (owlapi-defun (owlapi-disable-simplified-protocol)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disableSimplifiedProtocol|
           common-lisp-user::args))
  (owlapi-defun (owlapi-disable-memory-saving-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disableMemorySavingMode| common-lisp-user::args))
  (owlapi-defun (owlapi-disable-lookup-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disableLookupMode| common-lisp-user::args))
  (owlapi-defun (owlapi-disable-incremental-updates)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disableIncrementalUpdates|
           common-lisp-user::args))
  (owlapi-defun (owlapi-disable-auto-mode)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-disableAutoMode| common-lisp-user::args))
  (owlapi-defun (owlapi-describe-reasoners)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-describeReasoners| common-lisp-user::args))
  (owlapi-defun (owlapi-describe-reasoner)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-describeReasoner| common-lisp-user::args))
  (owlapi-defun (owlapi-describe-ontology)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-describeOntology| common-lisp-user::args))
  (owlapi-defun (owlapi-describe-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-describeOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-contains) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-contains| common-lisp-user::args))
  (owlapi-defun (owlapi-consider-declarations)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-considerDeclarations| common-lisp-user::args))
  (owlapi-defun (owlapi-clear-registry) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-clearRegistry| common-lisp-user::args))
  (owlapi-defun (owlapi-clear-ontologies)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-clearOntologies| common-lisp-user::args))
  (owlapi-defun (owlapi-clear-changes) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-clearChanges| common-lisp-user::args))
  (owlapi-defun (owlapi-classify) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-classify| common-lisp-user::args))
  (owlapi-defun (owlapi-batch-synchronize)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-batchSynchronize| common-lisp-user::args))
  (owlapi-defun (owlapi-auto-remove-axioms-from)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-autoRemoveAxiomsFrom| common-lisp-user::args))
  (owlapi-defun (owlapi-auto-batch-remove-axioms-from)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-autoBatchRemoveAxiomsFrom|
           common-lisp-user::args))
  (owlapi-defun (owlapi-auto-batch-add-axioms-to)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-autoBatchAddAxiomsTo| common-lisp-user::args))
  (owlapi-defun (owlapi-auto-apply-changes)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-autoApplyChanges| common-lisp-user::args))
  (owlapi-defun (owlapi-auto-add-axioms-to)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-autoAddAxiomsTo| common-lisp-user::args))
  (owlapi-defun (owlapi-apply-changes) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-applyChanges| common-lisp-user::args))
  (owlapi-defun (owlapi-add-prefix) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-addPrefix| common-lisp-user::args))
  (owlapi-defun (owlapi-add-axioms) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-addAxioms| common-lisp-user::args))
  (owlapi-defun (owlapi-add-axiom) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-addAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-set-ontology-uri)
                (&rest common-lisp-user::args)
    (apply #'|OWLAPI-SetOntologyURI| common-lisp-user::args))
  (owlapi-defun (owlapi-id-to-axiom) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-IDToAxiom| common-lisp-user::args))
  (owlapi-defun (owlapi-axiom-to-id) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-AxiomToID| common-lisp-user::args))
  (owlapi-defun (owlapi-axiom-loaded?) (&rest common-lisp-user::args)
    (apply #'|OWLAPI-AxiomLoaded?| common-lisp-user::args)))
