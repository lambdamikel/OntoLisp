;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

(in-package :owlapi)

;;;
;;;;  owlapi-test.lisp
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
;;;   Purpose: Demo and tests for the OWLAPI functionality. 
;;; 

(defvar *count* 0)

(defvar *demo* 0)

(defmacro demo (expr)
  `(progn 
    (incf *count*)
    (format t "~%~%Demo ~A, Expression ~A:~%" *demo* *count*)
    (pprint ',expr)
    (let ((res ,expr))
      (terpri)
      (princ " ===>")
      (pprint res)
      res)))
    
(defun owlapi-test ()
  (let ((*count* 0)
        (*demo* 0))

    (progn

      (incf *demo*)

      ;; Demo 1: Demonstrate basic OWLAPI managment

      ;; OWLAPI-init creates a default OWLAPI reasoner container called
      ;; "OWLAPI-KB" (and disposes all existing reasoners). A reasoner
      ;; container (reasoner for short) contains a set of ontology
      ;; containers (ontologies for short), and maintains a set of
      ;; axioms. Axioms can be added to or removed from an ontology of
      ;; that reasoner.  An ontology, wenn loaded into the description
      ;; logic reasoner, is loaded into a TBox / ABox whose name is
      ;; determined by the reasoner container. A reasoner also offers
      ;; some additional functionality, e.g. registry for entities,
      ;; etc.

      (demo (|OWLAPI-init|))

      (demo (|OWLAPI-getCurrentReasoner|))

      ;; a new ontology test is added to the current reasoner (default reasoner OWLAPI-KB) 
    
      (demo (|OWLAPI-newOntology| 'test))

      ;; another ontology is added to this reasoner. 

      (demo (|OWLAPI-newOntology| 'test2 (|OWLAPI-getCurrentReasoner|)))

      ;; we have two ontology containers now in the current reasoner:
    
      (demo (|OWLAPI-getOntologies|))

      (demo (|OWLAPI-getOntologies| '|OWLAPI-KB|))

      ;; we create an OWLClassAssertionAxiom. The syntax for individuals
      ;; and class expressions is the extended KRSS-syntax of the
      ;; RacerPro reasoner. The axiom constructor returns the axiom ID.
      ;; After creation, the axiom resides in the reasoner container.
      ;; Note that there is an axiom constructor for each OWL 2 axiom
      ;; type.  Please consult the source code. 

      (let ((axiom 
             (demo
              (|OWLAPI-getOWLClassAssertionAxiom|
               '(and c d)
               'i))))
      
        ;; to add an axiom to an ontology, we use its axiom ID.  An
        ;; axiom can be a member of multiple ontologies of the same
        ;; reasoner So, ontologies just "partition" axiom sets of a
        ;; reasoner container Ontologies can be loaded or unloaded. 

        (demo (|OWLAPI-AddAxiom| 'test axiom))
        (demo (|OWLAPI-AddAxiom| 'test2 axiom)))

      ;; as in the Java OWLAPI, adding or removing an axiom does not
      ;; automatically have an effect. Rather, a change object is
      ;; created and the changes have to be applied / performed in order
      ;; to become effective. So, the axiom will not be added to the 2
      ;; ontologies until we perform the changes as woll:

      (demo (|OWLAPI-getChanges|))

      (demo (|OWLAPI-applyChanges|))

      ;; rather than adding axioms by ID, we can also use the axiom's
      ;; constructor call. Again, we have to apply the changes: 

      (demo (|OWLAPI-AddAxiom| 'test2 
                               (|OWLAPI-getOWLSubClassAxiom| 'person 'animal)))

      (demo (|OWLAPI-getChanges|))

      (demo (|OWLAPI-applyChanges|))

      ;; there are various ways to look at the different types of axioms
      ;; in the reasoners and ontologies, please take a look at the source
      ;; for more functions: 
    
      (demo (|OWLAPI-getAxiomsIn| 'test))
      (demo (|OWLAPI-getAxiomsIn| 'test2))
      (demo (|OWLAPI-getAxioms|))
      (demo (|OWLAPI-getAxioms| (|OWLAPI-getCurrentReasoner|) t t :unloaded))

      ;; finally, we want to load the ontologies in the description
      ;; logic reasoner. A TBox / ABox pair "OWLAPI-KB" (the name of the
      ;; default reasoner) are created, and the axioms in the ontologies
      ;; of that reasoner can be loaded as follows. Note that this
      ;; brings up some "dummy function called" output. No DL reasoner
      ;; is supplied with this source. You will have to implement / replace
      ;; these "dummy functions" with the actual API calls of your DL 
      ;; reasoner: 

      (demo (|OWLAPI-loadOntology| 'test))
      (demo (|OWLAPI-loadOntology| 'test2))

      ;; an axiom can either be loaded or not loaded: 

      (demo (|OWLAPI-getAxioms| (|OWLAPI-getCurrentReasoner|) t t :loaded))

      ;; once the axioms are loaded into the DL reasoner, the OWLAPI
      ;; query functions can be used. Of course, they only return nil
      ;; here since no real DL reasoner is provided / attached. But note
      ;; that the dummy function retrieve-concept-instances is called.
      ;; There are many many more query functions. Please take a look at
      ;; the source code.

      (demo (|OWLAPI-getIndividuals| 'c nil))
      (demo (|OWLAPI-getIndividuals| 'd nil))

      ;; axioms can also be removed from an ontology container. Either
      ;; via its axioms ID, or by means of the construtor call. Note
      ;; that this doesn't dispose the axiom. It still exists in the
      ;; reasoner container and could be added again to the ontology
      ;; (however, there is also |OWLAPI-disposeAxiom|): 

      (demo (|OWLAPI-RemoveAxiom| 'test2 (|OWLAPI-getOWLSubClassAxiom| 'person 'animal)))
      (demo (|OWLAPI-getChanges|))
    
      (demo (|OWLAPI-applyChanges|))

      (demo (|OWLAPI-getAxiomsIn| 'test2))
    
      ;; no more changes:

      (demo (|OWLAPI-getChanges|))

      ;; a reasoner also maintains a simple registry:

      (let ((id 
             (demo (|OWLAPI-registerObject| '(or C top (or D X Y))))))

        (demo (|OWLAPI-getIndividuals| id nil)))

      ;; unloading an ontology means removing the axioms it contains
      ;; from the DL reasoner state:    

      (demo (|OWLAPI-unloadOntology| 'test))
  
      (demo (|OWLAPI-loadOntology| 'test))

      (demo (|OWLAPI-describeReasoner|))

      ;; dispose the ontology. Note that its axioms are not
      ;; automatically disposed.  After all, an axioms doesn't belong to
      ;; an ontology, but rather to the reasoner. An ontology may only
      ;; "contain" the axiom. In case the ontology is loaded and disposed, 
      ;; its axioms are automatically unloaded from the DL reasoner: 

      (demo (|OWLAPI-disposeOntology| 'test))

      ;; please note that for each |OWLAPI-...| function, there is
      ;; also a synonym function which has a name more suitable for
      ;; Lisp. The |OWLAPI-...| naming convention was taken to have a
      ;; close correspondance with the Java (2.2) OWLAPI. Please take
      ;; a look at owlapi-synonyms.lisp which contains these more
      ;; Lisp-like synonym functions:

      (demo (owlapi-get-axioms))

      ;; The ontology is gone: 

      (demo (owlapi-get-ontologies))

      ;; of course, we can dispose our reasoner as well (in case the
      ;; default reasoner, OWLAPI-KB, is disposed, it is simply
      ;; recreated)
    
      (demo (owlapi-dispose-reasoner (owlapi-get-current-reasoner)))

      (demo (owlapi-get-reasoners)))

  
    (progn 

      (incf *demo*)

      ;; Demo 2: having to use addAxiom / removeAxiom can be
      ;; cumbersome. Therefor, these additions / removals can be
      ;; performed automatically by enabling, for a given ontology of
      ;; a reasoner, the so-called "auto add" or "auto remove" mode.
      ;; Still, the add / remove changes for the ontologies must be
      ;; applied manually (but see below for more options): 

      (demo (|OWLAPI-init|))

      (demo (|OWLAPI-newReasoner| 'reasoner))

      (demo (|OWLAPI-getOntologies| 'reasoner))

      (demo (|OWLAPI-getCurrentReasoner|))

      (demo (|OWLAPI-newOntology| 'ontology))

      ;; here, we have now an ontology in the reasoner 

      (demo (|OWLAPI-getOntologies| 'reasoner))

      ;; we automatically want to add all freshly constructed
      ;; axioms for that reasoner to the ontology of that reasoner: 

      (demo (|OWLAPI-autoAddAxiomsTo| 'ontology))
      
      ;; if we load the ontology at that state (containing no axioms),
      ;; then this will ensure that each constructed axiom will not
      ;; only automatically be added to ontology, but also
      ;; automatically be loaded into the attached DL reasoner.
      ;; However, the changes will not be perfomed until applied:

      (demo (|OWLAPI-loadOntology| 'ontology))

      (demo (|OWLAPI-getOWLEquivalentClassesAxiom| '(parent
                                                     (and (some has-child person)))))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'parent
                                                'john))
    
      (demo (owlapi-get-changes 'reasoner))

      (demo (|OWLAPI-getOWLEquivalentClassesAxiom| '(parent
                                                     (and (some has-child person)))))

      ;; also in the auto add mode, axioms can be removed or added manually: 

      (demo 
       (|OWLAPI-RemoveAxiom|
        'ontology
        (|OWLAPI-getOWLEquivalentClassesAxiom| '(parent
                                                 (and (some has-child person))))))

      (demo 
       (|OWLAPI-AddAxiom|
        'ontology
        (|OWLAPI-getOWLEquivalentClassesAxiom| '(parent
                                                 (and (some has-child person))))))

      
      (demo (|OWLAPI-getChanges|))
    
      (demo (|OWLAPI-applyChanges|))

      ;; the analog of the auto add mode is the auto remove mode. Here, each constructor
      ;; call identified the axiom to be removed. Note that axioms are identified by
      ;; the rules of OWL 2 structural equivalence. However, in case the axiom is not
      ;; found in the ontology, an error is signaled:

      (demo (|OWLAPI-autoRemoveAxiomsFrom| 'ontology))

      (demo (|OWLAPI-getOWLEquivalentClassesAxiom| '(parent
                                                     (and (some has-child person)))))
    
      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'parent 'john))

      (demo (|OWLAPI-getAxiomsIn| 'ontology))
 
      (demo (|OWLAPI-getAxioms| 'reasoner))

      ;; Axioms can also be identified via their IDs, as demonstrated before: 
    
      (demo (|OWLAPI-autoAddAxiomsTo| 'ontology))

      (demo (|OWLAPI-getOWLEquivalentClassesAxiom| '(parent
                                                     (and (some has-child person)))))
    
      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'parent 'john))

      (demo (|OWLAPI-getAxiomsIn| 'ontology 'reasoner t))

      (demo (|OWLAPI-autoRemoveAxiomsFrom| 'ontology))

      (demo (|OWLAPI-RemoveAxiom| 'ontology 3))
    
      (demo (|OWLAPI-RemoveAxiom| 'ontology 4))

      (demo (|OWLAPI-getChanges|))

      (demo (|OWLAPI-applyChanges|))

      (demo (|OWLAPI-getAxiomsIn| 'ontology 'reasoner t))

      (demo (|OWLAPI-disposeReasoner| 'reasoner)))
  
    (progn 

      (incf *demo*) 

      ;; Demo 3: Per default, axioms are maintained as structures.
      ;; For huge ontologies, this memory footprint may be too large.
      ;; Hence, the memory saving mode doesn't maintain the axiom
      ;; objects, but rather, each constructor call results directly
      ;; in a call of the corresponding DL reasoner function. Of
      ;; course, ontologies and change objects become rather
      ;; pointless then, too. Morever, in the memory saving modew
      ;; axiom removal / incremental unloaded etc. become impossible,
      ;; and also OWL rendering will not work (the told information
      ;; is lost):
    
      (demo (|OWLAPI-init|))
    
      (demo (|OWLAPI-newOntology| 'ontology))

      (demo (|OWLAPI-enableMemorySavingMode| 'ontology))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'i 'c))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'j 'd))

      (demo (|OWLAPI-describeReasoner|))

      (demo (owlapi-get-changes))

      (demo (owlapi-get-axioms)))

    (progn 

      (incf *demo*)

      ;; Demo 4: Also change objects require some memory.  There is a
      ;; specifialized auto mode called the "batch" mode where change
      ;; objects are not explicitly created (so they cannot be
      ;; inspected and applied etc.), but rather are maintained much
      ;; more efficently and less memory hungry internally. Instead
      ;; of |OWLAPI-applyChanges|, |OWLAPI-batchSynchronize| has to
      ;; be used to perform the (internal) changes to the ontologies:
    
      (demo (|OWLAPI-init|))

      (demo (|OWLAPI-newOntology| 'ontology))

      (demo (|OWLAPI-autoBatchAddAxiomsTo| 'ontology))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'i 'c))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'j 'd))

      (demo (|OWLAPI-batchSynchronize| 'ontology))

      (demo (|OWLAPI-autoBatchRemoveAxiomsFrom| 'ontology))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'i 'c))

      (demo (|OWLAPI-getOWLClassAssertionAxiom| 'j 'd))

      (demo (|OWLAPI-batchSynchronize| 'ontology))

      (demo (owlapi-get-axioms)))

    (progn 

      (incf *demo*) 

      ;; Demo 4: Finally, let us demonstrate that axioms may also be
      ;; constructed by utilizing the OWL 2 functional parser. Please
      ;; note that more OWL 2 parsing and rendering stuff is presented
      ;; in owl-syntaxes-test.lisp. This time I am using primarily the
      ;; more Lisp-friendly OWLAPI synonym functions rather than the 
      ;; |OWLAPI-...| functions: 

      (demo (owlapi-init))

      (demo (owlapi-new-ontology 'ontology))

      (demo (owlapi-auto-add-axioms-to 'ontology))

      (demo (owlapi-get-auto-ontology))
    
      (let ((*package* (find-package :owl-syntaxes)))

        (demo (owlapi-parse-native "SubClassOf(C D)"))
      
        (demo (owlapi-parse-native "EquivalentClasses(
     :HappyPerson
     ObjectIntersectionOf(
        ObjectAllValuesFrom( :hasChild :HappyPerson )
        ObjectSomeValuesFrom( :hasChild :HappyPerson )
     ))")))
    
      (demo (owlapi-apply-changes))

      (demo (owlapi-get-axioms))

      (demo (owlapi-load-ontology 'ontology))

      ;; in fact, it is even possible to construct whole ontologies
      ;; and their axioms in that way. However, there are some package
      ;; issues still: 
  
      (let ((*package* (find-package :owl-syntaxes)))

        (demo (owlapi-parse-native "Ontology(<test>
                           SubClassOf(C D)
                           EquivalentClasses(
                              :HappyPerson
                              ObjectIntersectionOf(
                                ObjectAllValuesFrom( :hasChild :HappyPerson )
                                                   ObjectSomeValuesFrom( :hasChild :HappyPerson ))))")))

      ;; note that this is the ontolgy the parser has just created and
      ;; populated with axioms:

      (demo (owlapi-get-ontologies))

      (demo (owlapi-load-ontology 'owl-syntaxes::|test|))

      (demo (owlapi-dispose-reasoner (owlapi-get-current-reasoner))))

    (progn 

      ;; Now, please continue to consult the examples in owl-syntaxes-test.lisp
      ;; to learn about the OWL 2 parsing and rendering facilities of OntoLisp. 
      ;; This also includes the OWLlink implementation. 

      t)))
    
    
    
