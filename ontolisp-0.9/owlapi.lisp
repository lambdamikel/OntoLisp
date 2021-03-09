;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

(in-package :owlapi)

;;;
;;;;  owlapi.lisp
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
;;;   Purpose: The basic ontology management framework of OntoLisp 
;;;            (inspired by the Java OWLAPI v2.2) 
;;; 

#+:racer-server
(declaim (special ts::*really-warn-p*))

(defvar *reasoners* (make-hash-table :test #'eql))

(defvar *temp-ont-counter* 0)

(defvar *cur-reasoner* nil)

(defvar *default-reasoner* nil)

(defparameter *default-reasoner-name* 
  #+:racer-server
  'racer-user::OWLAPI-KB
  #-:racer-server
  'OWLAPI-KB)

(defparameter *register-referenced-entities-p* nil)

(defparameter *register-declared-entities-p* nil)

;;;
;;;
;;;

(define-constant +owl2-namespace+ "http://www.w3.org/2002/07/owl#")

(define-constant +old-owl2-namespace+ "http://www.w3.org/2006/12/owl2-xml#")

(define-constant +owlapi-owl-thing+ 
  #+:racer-server (intern racer:+owl-thing+)
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#Thing"))

(define-constant +owlapi-owl-nothing+ 
  #+:racer-server (intern racer:+owl-nothing+)
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#Nothing"))

(define-constant +owlapi-owl-top+ 
  #+:racer-server (intern racer:+owl-thing+)
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#Thing"))

(define-constant +owlapi-owl-bottom+ 
  #+:racer-server (intern racer:+owl-nothing+)
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#Nothing"))

(define-constant +owlapi-owl-top-object-role+ 
  #+:racer-server racer:+owl-top-object-role+
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#topObjectProperty"))

(define-constant +owlapi-owl-bottom-object-role+ 
  #+:racer-server racer:+owl-bottom-object-role+
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#bottomObjectProperty"))

(define-constant +owlapi-owl-top-data-role+ 
  #+:racer-server racer:+owl-top-data-role+
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#topDataProperty"))

(define-constant +owlapi-owl-bottom-data-role+ 
  #+:racer-server racer:+owl-bottom-data-role+
  #-:racer-server (intern "http://www.w3.org/2002/07/owl#bottomDataProperty"))

;;;
;;;
;;; 

(define-constant +defaultnamespace-prefixes+
  '(:|defaultnamespace|
    :defaultnamespace
    :\: 
    :|| ;; (intern \"\" ) = ||
    nil))

(defun is-default-prefix-p (prefix)
  (let ((prefix (ensure-string prefix)))
    (member prefix +defaultnamespace-prefixes+
            :key #'symbol-name
            :test #'string-equal)))

;;;
;;;
;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *structure-slots* nil)

  (defmacro defconstructor (name (&rest args) &body body)
    `(owlapi-defun (,name) (,@args &optional reasoner)
      #|(let ,(mapcar #'(lambda (x) 
                         (list x 
                               `(if (consp ,x)
                                    (copy-tree ,x)
                                  ,x)))
                     args) |# 
         ,@body))

  (defmacro defaxiom (name superclasses slots)

    (dolist (slot slots)
      (let ((syms 
             (remove-duplicates
              (remove nil
                     (list 
                      (first slot)
                      (second (member :initarg slot))
                      (second (member :reader slot))
                      (second (member :writer slot))
                      (second (member :accessor slot))))
              :test #'(lambda (x y) 
                        (string-equal (symbol-name x)
                                      (symbol-name y))))))
        (when (cdr syms)
          (error "Bad slot: ~S, use same symbol instead of ~S" 
                 slot  syms))))
    
    (push (list name 
		(append 
		 (reduce #'append
			 (mapcar
			  (lambda (superclass)
			    (second (assoc superclass *structure-slots*)))
			  superclasses))
		 slots))
	  *structure-slots*)

    (let* ((redefined-slots 
	    (intersection slots
			  (when superclasses
			    (second (assoc (first superclasses) *structure-slots*)))
			  :test #'equal
			  :key #'first))
	   (form 
	    `(progn 
	       (defpersistentstruct 
                (,name
                 ,@(when superclasses 
                     (when (cdr superclasses)
                       (error "Can only include one structure: ~S" superclasses))
                     `((:include ,@superclasses 
                        ,@(mapcar #'(lambda (slot)
                                      (list
                                       (first slot)
                                       (second (member :initform slot))))
                                  redefined-slots))))
                 (:conc-name)
                 (:package :owlapi))
		 ,@(mapcar #'(lambda (slot)
			       (list
				(first slot)
				(second (member :initform slot))))
			   (set-difference slots
					   (when superclasses
					     (second (assoc (first superclasses) *structure-slots*)))
					   :test #'equal
					   :key #'first))))))

      ;; (pprint form)

      form)))

(defun clear-last-answer
       ()
  (when *cur-reasoner*
    (setf (last-answer *cur-reasoner*) :void)))
  
(defmacro return-by-policy ()
  `(progn 
     (when *cur-reasoner*
       (labels ((trafo (x) 
		  (typecase x 
		    (|OWLAxiom|
		     (unparse x))
		    (|OWLOntologyChange|
		     (unparse x))
		    (cons 
                     (if (not (cdr (last x))) ; echt Liste? 
                         (mapcar #'trafo x)
                       (append (mapcar #'trafo (butlast x))
                               (cons (car (last x))
                                     (cdr (last x))))))
		    (otherwise 
		     x))))
	 (let ((ans 
		(last-answer *cur-reasoner*)))
	   (if (listp ans)
	       (mapcar #'trafo ans)
	     (trafo ans)))))))
	       
(defmacro return-check-if-abox-consistent ()
  `(progn 
     (let ((val
            (reasoner-abox-coherent-p (owlapi-abox *cur-reasoner*))))       
       (cond ((eq val :down-know)
              (return-by-policy))
             ((eq val t)
              (return-by-policy))
             ((eq val :inconsistent) 
              (setf (last-answer *cur-reasoner*)
                    :abox-inconsistent)
              (return-by-policy))))))

;;;
;;;
;;;

(defmethod (setf last-answer) (val (x null))
  (declare (ignore val))
  (owlapi-runtime-error "OWLAPI not initialized - try \"(OWLAPI-init)\" first"))

(defmethod last-answer ((x null))
  (declare (ignore val))
  (owlapi-runtime-error "OWLAPI not initialized - try \"(OWLAPI-init)\" first"))

;;;
;;;
;;;

(defmacro clear-entities (&rest entities)
  `(progn 
     ,@(mapcar #'(lambda (val)
                   `(setf ,val (remove-entity-if-present ,val)))
               entities)))

(defmacro clear-entities1 (&rest entities)
  `(progn 
     ,@(mapcar #'(lambda (val)
                   `(setf ,val (remove-entities-if-present ,val)))
               entities)))

(defun remove-entity-if-present (val)
  (if (consp val)
      (case (first val)
        ((|OWLClass| 
          |Class| 
          |ObjectProperty| 
          |DataProperty| 
          |Annotation|
          |Datatype|
          |AnnotationProperty| 
          |Individual|
          |NamedIndividual|)
         (second val))
        (otherwise val))
    val))

(defun remove-entities-if-present (val)
  (if (consp val)
      (mapcar #'remove-entity-if-present val)
    val))

;;;
;;; OWLReasoner 
;;;


(defpersistentclass reasoner ()
  ((owlapi-reasoner-name :accessor owlapi-reasoner-name :initform *default-reasoner-name*
                         :initarg :owlapi-reasoner-name)

   (owlapi-tbox :accessor owlapi-tbox :initform *default-reasoner-name*
                :initarg :owlapi-tbox)
   (owlapi-abox :accessor owlapi-abox :initform *default-reasoner-name*
                :initarg :owlapi-abox)

   (namespace-table-stack :accessor namespace-table-stack :initform nil)

   (namespace-table :accessor namespace-table 
                    :initform 
                    (let ((ht (mht :size 10 :test #'equalp)))
                      (reasoner-delete-prefix-mappings)
                      (loop as (prefix namespace) in 
                            (reasoner-get-prefixes (reasoner-current-tbox) nil)
                            do
                            (let ((prefix 
                                   (or prefix  :defaultnamespace)))
			      
			      (when (is-default-prefix-p prefix)
				(dolist (prefix +defaultnamespace-prefixes+)
				  (setf (gethash prefix 
						 (namespace-table *cur-reasoner*))
				    namespace)))
			      
                              (setf (gethash 
                                     (to-keyword 
                                      (ensure-ends-with-colon prefix))
                                     ht)
                                    namespace)))
                      ht))

   (return-policy :accessor return-policy :initform :smart)
   (simple-output :accessor simple-output :initform nil)
   (incremental-updates :accessor incremental-updates :initform t)

   (last-answer :accessor last-answer :initform :void)
   (last-error :accessor last-error :initform nil)
   (last-output-stream-string :accessor last-output-stream-string :initform nil)

   (ontologies :accessor ontologies :initform (mht :size 10 :test #'equalp))
   (axioms :accessor axioms :initform (mht :test #'eql))
   (axioms-told :accessor axioms-told :initform (mht :test #'equalp))
   (axiom-counter :accessor axiom-counter :initform 0) 
   (next-axiom-use-id :accessor next-axiom-use-id :initform nil) 
    
   (changes :accessor changes :initform nil)

   (id-to-object-table :accessor id-to-object-table :initform 
                       (mht :size 1000 :test #'eql))
   (object-to-id-table :accessor object-to-id-table :initform 
                       (mht :size 1000 :test #'equalp))
   (id-counter :accessor id-counter :initform 0)

   (lookup-mode :accessor lookup-mode :initform nil) ; use axiom constructor calls just for lookup of existing axioms
   (transient-mode :accessor transient-mode :initform nil) ; create axiom but dont add to reasoner or ontology 
   (auto-mode :accessor auto-mode :initform nil) ; add or remove automatically to / from ontology
   (auto-apply :accessor auto-apply :initform nil) ; auto apply changes (not applyChanges required if t) 
   (auto-declare-datatype-properties-p  :accessor auto-declare-datatype-properties-p :initform t)
   (dont-keep-axioms-p :accessor dont-keep-axioms-p :initform nil)
   (ignore-annotations-p :accessor ignore-annotations-p :initform nil)
   (ignore-declarations-p :accessor ignore-declarations-p :initform nil)

   (register-referenced-entities-p :accessor register-referenced-entities-p :initform *register-referenced-entities-p*)
   (register-declared-entities-p :accessor register-declared-entities-p :initform *register-declared-entities-p*)
   (used-constructors :accessor used-constructors :initform nil)))
   

(defmethod pop-namespace-table ((reasoner reasoner))
  (reasoner-reset-prefix-cache)
  (setf (namespace-table reasoner)
        (pop (namespace-table-stack reasoner))))

(defmethod push-namespace-table ((reasoner reasoner))
  (reasoner-reset-prefix-cache)
  (push (namespace-table reasoner)
        (namespace-table-stack reasoner))
  (setf (namespace-table reasoner)
        (let ((ht (mht :size 10 :test #'equalp)))
          (maphash #'(lambda (prefix namespace) 
                       (setf (gethash prefix ht) namespace))
                   (namespace-table reasoner))
          ht)))

(defmethod get-ontology-names ((reasoner reasoner) &optional (error-p t))
  (declare (ignorable error-p))
  (mapcar #'name (get-ontologies reasoner)))

(defmethod get-ontology-names ((reasoner symbol) &optional (error-p t))
  (let ((reasoner 
         (find-reasoner reasoner error-p)))
    (if reasoner
        (get-ontology-names reasoner)
      nil)))


(defmethod get-ontologies ((reasoner reasoner) &optional (error-p t))
  (declare (ignorable error-p))
  (remove-duplicates 
   (loop as ont being the hash-value of (ontologies reasoner)
         collect ont)))

(defmethod get-ontologies ((reasoner symbol) &optional (error-p t))
  (let ((reasoner 
         (find-reasoner reasoner error-p)))
    (if reasoner
        (get-ontologies reasoner)
      nil)))

(defmethod get-all-ontologies ()
  (declare (ignorable error-p))
  (loop as name being the hash-key of *reasoners*
        as reasoner being the hash-value of *reasoners*
        collect
        `(:reasoner 
          ,name
          (:ontologies
           ,@ (mapcar #'(lambda (ont)
                          `(:name ,(name ont) :aka ,@(remove (name ont) 
                                                             (all-names ont))))
                      (remove-duplicates 
                       (loop as ont being the hash-value of (ontologies reasoner) collect ont)))))))
           
;;;
;;; General Purpose Registry
;;; 

(defun create-id ()
  (incf (id-counter *cur-reasoner*)))

(defun get-current-id ()
  (id-counter *cur-reasoner*))

(defun reset-id-counter ()
  (setf (id-counter *cur-reasoner*) 0))

;;;
;;;
;;;

(defmacro with-reasoner ((reasoner) &body body)
  `(let ((*cur-reasoner*
          (find-reasoner (or ,reasoner *cur-reasoner*))))
     ,@body))

(defmethod initialize-instance :after ((reasoner reasoner) &rest initargs)
  (declare (ignorable initargs))
  (setf (gethash (owlapi-reasoner-name reasoner) *reasoners*) reasoner)
  (when (eq (owlapi-reasoner-name reasoner) *default-reasoner-name*)
    (setf *default-reasoner* (find-reasoner (owlapi-reasoner-name reasoner)))))
  

(owlapi-defun (|OWLAPI-newReasoner|) (&optional (owlapi-reasoner-name *default-reasoner-name*) 
					      make-racer-kb-current-p
                                              (init t)
                                              owlapi-tbox
                                              owlapi-abox
                                              )

  (let ((owlapi-reasoner-name
         (etypecase owlapi-reasoner-name
           (string
            (intern (ensure-string owlapi-reasoner-name)))
           (symbol 
            owlapi-reasoner-name))))
	      
    (let* ((abox (reasoner-current-abox))
           (tbox (reasoner-current-tbox))
           (reasoner (find-reasoner owlapi-reasoner-name nil)))

      (when (or init (not reasoner))
        (make-instance 'reasoner :owlapi-reasoner-name owlapi-reasoner-name))

      (with-reasoner (owlapi-reasoner-name)
      
        (setf (owlapi-abox *cur-reasoner*) 
              (if (and (not init) reasoner)
                  ;; namen beibehalten
                  (owlapi-abox reasoner)
                (or owlapi-abox 
                    owlapi-reasoner-name
                    abox)))
      
        (setf (owlapi-tbox *cur-reasoner*)
              (if (and (not init) reasoner)
                  (owlapi-tbox reasoner)
                (or owlapi-tbox 
                    owlapi-reasoner-name
                    tbox)))

        (reasoner-new-tbox-and-abox (owlapi-tbox *cur-reasoner*) (owlapi-abox *cur-reasoner*) init))
        
      (|OWLAPI-setCurrentReasoner| owlapi-reasoner-name make-racer-kb-current-p)
      
      (unless make-racer-kb-current-p 
	(reasoner-set-current-tbox tbox)
	(reasoner-set-current-abox abox))
          
      (setf (last-answer (find-reasoner owlapi-reasoner-name)) owlapi-reasoner-name)
      
      (return-by-policy))))
  


(defun find-reasoner (name &optional (error-p t))
  (if (typep name 'reasoner)
      name
    (or (gethash name *reasoners*)
        (when error-p 
	  (owlapi-runtime-error "Can't find reasoner ~A" name)))))

(owlapi-defun (|OWLAPI-disposeReasoner|) (name)
  (let ((reasoner (find-reasoner name)))

    (reasoner-dispose reasoner)
    (remhash (owlapi-reasoner-name reasoner) *reasoners*)

    (when (eq *cur-reasoner* reasoner)
      (setf *cur-reasoner* *default-reasoner*))

    (when (eq reasoner *default-reasoner*)
      (|OWLAPI-newReasoner|))

    (clear-last-answer)
    
    (return-by-policy)))


(owlapi-defun (|OWLAPI-setCurrentReasoner|) (name &optional make-racer-kb-current-p)
  (setf *cur-reasoner*
        (find-reasoner name))
  
  (when make-racer-kb-current-p
    (reasoner-set-current-tbox (owlapi-tbox *cur-reasoner*))
    (reasoner-set-current-abox (owlapi-abox *cur-reasoner*)))

  (clear-last-answer)
  (return-by-policy))
        

(owlapi-defun (|OWLAPI-getCurrentReasoner|) ()
  (setf (last-answer *cur-reasoner*) (owlapi-reasoner-name *cur-reasoner*))
  (return-by-policy))

(owlapi-defun (|OWLAPI-getReasoners|) ()
  (setf (last-answer *cur-reasoner*)
        (loop as name being the hash-key of *reasoners*
              collect name))
  (return-by-policy))

          
;;;
;;;
;;;  

(owlapi-defun (|OWLAPI-init|) ()
  (|OWLAPI-dispose|)
  (|OWLAPI-newReasoner|)
  (clear-last-answer)
  (return-by-policy))

;;;
;;; 
;;;

#+:racer-server
(owlapi-defun (|OWLAPI-enableSimplifiedProtocol|) 
  (&optional reasoner)
  (when (eq reasoner :|global|)
      (setf user:*one-simple-output* t)
      (setf reasoner nil))
  (with-reasoner (reasoner)
    (setf (simple-output *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))

#+:racer-server
(owlapi-defun (|OWLAPI-disableSimplifiedProtocol|) 
  (&optional reasoner)
  (when (eq reasoner :|global|)
    (setf user:*one-simple-output* nil)
    (setf reasoner nil))
  (with-reasoner (reasoner)
    (setf (simple-output *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

#+:racer-server
(owlapi-defun (|OWLAPI-usesSimplifiedProtocol|) 
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*)
          (or user:*one-simple-output* 
              (simple-output *cur-reasoner*)))
    (return-by-policy)))


;;;
;;; 
;;;

(owlapi-defun (|OWLAPI-enableIncrementalUpdates|) 
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (incremental-updates *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-disableIncrementalUpdates|) 
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (incremental-updates *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-usesIncrementalUpdates|) 
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*)
          (incremental-updates *cur-reasoner*))
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-registerReferencedEntities|) 
  (&optional reasoner)
  (if (eq reasoner :|global|)
      (setf *register-referenced-entities-p* t)
    (with-reasoner (reasoner)
      (setf (register-referenced-entities-p *cur-reasoner*) t)
      (clear-last-answer)
      (return-by-policy))))

(owlapi-defun (|OWLAPI-dontRegisterReferencedEntities|) 
  (&optional reasoner)
  (if (eq reasoner :|global|)
      (setf *register-referenced-entities-p* nil)
    (with-reasoner (reasoner)
      (setf (register-referenced-entities-p *cur-reasoner*) nil)
      (clear-last-answer)
      (return-by-policy))))

;;;
;;;
;;;

(owlapi-defun (|OWLAPI-registerDeclaredEntities|) 
  (&optional reasoner)
  (if (eq reasoner :|global|)
      (setf *register-declared-entities-p* t)
    (with-reasoner (reasoner)
      (setf (register-declared-entities-p *cur-reasoner*) t)
      (clear-last-answer)
      (return-by-policy))))

(owlapi-defun (|OWLAPI-dontRegisterDeclaredEntities|) 
  (&optional reasoner)
  (if (eq reasoner :|global|)
      (setf *register-declared-entities-p* nil)
    (with-reasoner (reasoner)
      (setf (register-declared-entities-p *cur-reasoner*) nil)
      (clear-last-answer)
      (return-by-policy))))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-nextAxiomUseID|) 
  (id &optional reasoner)
  (with-reasoner (reasoner)
    (setf (next-axiom-use-id *cur-reasoner*) id)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;;

#|

(owlapi-defun (|OWLAPI-OWLXMLParserCreateAxioms|) 
  ()
  (setf *use-owlapi* t)
  (clear-last-answer)
  (return-by-policy))

(owlapi-defun (|OWLAPI-OWLXMLParserDontCreateAxioms|)
  ()
  (setf *use-owlapi* nil)
  (clear-last-answer)
  (return-by-policy))

|#

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-setReturnPolicy|) 
  (type &optional reasoner)
  (with-reasoner (reasoner)

    (setf (return-policy *cur-reasoner*) 
          (ecase type
            (:answer-direct type)
            (:get-last-answer type)
            (:smart type))))

  (clear-last-answer)
  
  (return-by-policy))


(owlapi-defun (|OWLAPI-clearRegistry|)
  (&optional reasoner)

  (with-reasoner (reasoner)

    ;; (reset-id-counter)
    (clrhash (id-to-object-table *cur-reasoner*))
    (clrhash (object-to-id-table *cur-reasoner*))

    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-registerLastAnswer|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (|OWLAPI-registerObject| (last-answer *cur-reasoner*))
    (clear-last-answer)
    (return-by-policy)))
    

(owlapi-defun (|OWLAPI-registerObject|)
  (obj)
  (setf (gethash (create-id) (id-to-object-table *cur-reasoner*))
        obj)
  (setf (gethash obj (object-to-id-table *cur-reasoner*))
        (get-current-id))
  
  (setf (last-answer *cur-reasoner*)
        (get-current-id))

  (return-by-policy))

(owlapi-defun (|OWLAPI-findObjectFromID|)
  (id)
  (gethash id (id-to-object-table *cur-reasoner*)))

(owlapi-defun (|OWLAPI-findIDFromObject|)
  (obj)
  (setf (last-answer *cur-reasoner*) 
        (gethash obj (object-to-id-table *cur-reasoner*)))

  (return-by-policy))

;;;
;;; 
;;; 

(defun find-owl-class (cls-or-id)
  (if (integerp cls-or-id)
      (or (gethash cls-or-id (id-to-object-table *cur-reasoner*))
	  cls-or-id)
    cls-or-id))

(defun find-owl-individual (ind-or-id)
  (if (integerp ind-or-id)
      (or (gethash ind-or-id (id-to-object-table *cur-reasoner*))
	  ind-or-id)
    ind-or-id))

(defun find-owl-property (prop-or-id)
  (if (integerp prop-or-id)
      (or (gethash prop-or-id (id-to-object-table *cur-reasoner*))
	  prop-or-id)
    prop-or-id))

(defun find-owl-entity (entity-or-id) ; anything (CLass, Ind, Property, ...) 
  (if (integerp entity-or-id)
      (or (gethash entity-or-id (id-to-object-table *cur-reasoner*))
	  entity-or-id)
    entity-or-id))

;;;
;;;
;;;

(defun declare-datatype-properties (concept)
  (when (auto-declare-datatype-properties-p *cur-reasoner*)
    (if (not (consp concept))
        concept
      (case (first concept)
        ((d-some d-all)
         (reasoner-role-is-used-as-datatype-property (second concept)
                                                            (owlapi-tbox *cur-reasoner*))
         (declare-datatype-properties (third concept)))
        ((d-at-least d-at-most d-exactly)
         (reasoner-role-is-used-as-datatype-property (third concept)
                                                            (owlapi-tbox *cur-reasoner*))
         (declare-datatype-properties (third concept)))
        (otherwise 
         (dolist (concept (rest concept))
           (declare-datatype-properties concept)))))))

;;;
;;; OWLClassReasoner 
;;;

(owlapi-defun (|OWLAPI-isClass|)
  (clsC &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)

    (setf (last-answer *cur-reasoner*) 
          (reasoner-concept-p (find-owl-class clsC) (owlapi-tbox *cur-reasoner*)))

    (return-by-policy)))

(owlapi-defun (|OWLAPI-isSubClassOf|)
  (clsC clsD &optional reasoner)  
  ;; boolean
  (with-reasoner (reasoner)
    (let ((clsC (find-owl-class clsC))
          (clsD (find-owl-class clsD)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-concept-subsumes-p clsD clsC (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-isEquivalentClass|) 
  (clsC clsD &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)
    (let ((clsC (find-owl-class clsC))
          (clsD (find-owl-class clsD)))
    
      (setf (last-answer *cur-reasoner*) 
            (reasoner-concepts-equivalent-p clsC clsD (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))


(owlapi-defun (|OWLAPI-getSuperClasses|) 
  (cls &optional reasoner)
  ;; Set<Set<OWLClass>>
  (with-reasoner (reasoner)
    (let ((cls (find-owl-class cls)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-concept-parents cls (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-getAncestorClasses|)
  (cls &optional reasoner)
  ;; Set<Set<OWLClass>>
  (with-reasoner (reasoner)
    (let ((cls (find-owl-class cls)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-concept-ancestors cls (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-getSubClasses|)
  (cls &optional reasoner)
  ;; Set<Set<OWLClass>>
  (with-reasoner (reasoner)
    (let ((cls (find-owl-class cls)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-concept-children cls (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))
  
(owlapi-defun (|OWLAPI-getDescendantClasses|)
  (cls &optional reasoner)
  ;; Set<Set<OWLClass>>
  (with-reasoner (reasoner)
    (let ((cls (find-owl-class cls)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-concept-descendants cls (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))
  
(owlapi-defun (|OWLAPI-getEquivalentClasses|)
  (cls &optional reasoner)
  ;; Set<OWLClass>
  (with-reasoner (reasoner)
    (let ((cls (find-owl-class cls)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-equivalent-concepts cls (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-getInconsistentClasses|)
  (&optional reasoner)   
  ;; Set<OWLClass>
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) 
          (reasoner-inconsistent-concepts (owlapi-tbox *cur-reasoner*)))

    (return-by-policy)))
  
;;;
;;; OWLConsistencyChecker
;;;

(owlapi-defun (|OWLAPI-isConsistent|)
  (ontology &optional reasoner)  
  ;; boolean
  
  (with-reasoner (reasoner)
    (|OWLAPI-loadOntologies| (list ontology))

    (setf (last-answer *cur-reasoner*)
          (reasoner-kb-is-consistent-p (owlapi-tbox *cur-reasoner*)
                                       (owlapi-abox *cur-reasoner*)))
    
    (return-by-policy)))

;;;
;;; OWLIndividualReasoner 
;;; 

(owlapi-defun (|OWLAPI-getTypes|)
  (individual direct &optional reasoner)
  ;; Set<Set<OWLClass>>
  (with-reasoner (reasoner)
    (let ((ind (find-owl-individual individual)))
      (setf (last-answer *cur-reasoner*)
            (reasoner-get-types ind (owlapi-abox *cur-reasoner*) direct))
        
      (return-check-if-abox-consistent))))

(owlapi-defun (|OWLAPI-getIndividuals|)
  (class direct &optional reasoner)
  ;; Set<OWLIndividual>
  (with-reasoner (reasoner)
    (let* ((class (find-owl-class class)))
      (setf (last-answer *cur-reasoner*)
            (reasoner-get-instances class (owlapi-abox *cur-reasoner*) direct))
          
      (return-check-if-abox-consistent))))

(owlapi-defun (|OWLAPI-getObjectPropertyRelationships|)
    (ind &optional reasoner)
  ;; Map<OWLObjectProperty, Set<OWLIndividual>> 
  (with-reasoner (reasoner)
    (let ((ind (find-owl-individual ind)))
      (setf (last-answer *cur-reasoner*)
            (reasoner-get-individual-successors ind
                                                (owlapi-abox *cur-reasoner*)
                                                :remove-synonyms-p nil))

      (return-check-if-abox-consistent))))


(owlapi-defun (|OWLAPI-getDataPropertyRelationships|)
  (ind &optional reasoner)
  ;; Map<OWLDataProperty, Set<OWLConstant>>
  (with-reasoner (reasoner)
    (let ((ind (find-owl-individual ind)))
      (setf (last-answer *cur-reasoner*)
            (reasoner-get-individual-datatype-fillers ind (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))
  
(owlapi-defun (|OWLAPI-hasType|)
  (ind type direct &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)
    (let ((ind (find-owl-individual ind))
          (type (find-owl-class type)))

        (setf (last-answer *cur-reasoner*)
              (reasoner-instance-of-p ind type (owlapi-abox *cur-reasoner*) direct))

      (return-check-if-abox-consistent))))

  
(owlapi-defun (|OWLAPI-hasObjectPropertyRelationship|)
  (subject property object &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)
    (let ((subject (find-owl-individual subject))
          (object (find-owl-individual object))
          (property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-individuals-related-p subject  
                                            object 
                                            property
                                            (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))

(owlapi-defun (|OWLAPI-hasDataPropertyRelationship|)
  (subject property object &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)
    (let ((subject (find-owl-individual subject))
          (object (find-owl-individual object))
          (property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-individual-has-data-filler-p subject object property (owlapi-abox *cur-reasoner*)))
      
      (return-check-if-abox-consistent))))


(owlapi-defun (|OWLAPI-getRelatedIndividuals|)
  (subject object-property &optional reasoner)
  ;; Set<OWLIndividual>
  (with-reasoner (reasoner)
    (let ((subject (find-owl-individual subject))
          (object-property (find-owl-property object-property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-retrieve-individual-fillers subject
                                                  object-property
                                                  (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))
  

(owlapi-defun (|OWLAPI-getRelatedValues|)
  (subject data-property &optional reasoner)
  ;; Set<OWLConstant>
  (with-reasoner (reasoner)
    (let ((subject (find-owl-individual subject))
          (data-property (find-owl-property data-property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-retrieve-individual-told-datatype-fillers subject
                                                                data-property
                                                                (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))

(owlapi-defun (|OWLAPI-getSameIndividuals|) 
  (ind &optional reasoner)
  ;; Set<OWLIndividual>
  (with-reasoner (reasoner)
    (let ((ind (find-owl-individual ind)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-get-synonym-individuals ind (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))


(owlapi-defun (|OWLAPI-isSameIndividual|) 
  (i j &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)
    (let ((i (find-owl-individual i))
          (j (find-owl-individual j)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-synonym-individuals-p i j (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))


(owlapi-defun (|OWLAPI-getDifferentIndividuals|) 
  (ind &optional reasoner)
  ;; Set<OWLIndividual>
  (with-reasoner (reasoner)
    (let ((ind (find-owl-individual ind)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-get-antonym-individuals ind (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))


(owlapi-defun (|OWLAPI-isDifferentIndividual|) 
  (i j &optional reasoner)
  ;; boolean
  (with-reasoner (reasoner)
    (let ((i (find-owl-individual i))
          (j (find-owl-individual j)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-antonym-individuals-p i j (owlapi-abox *cur-reasoner*)))

      (return-check-if-abox-consistent))))


;;;
;;; OWLPropertyReasoner
;;;

(owlapi-defun (|OWLAPI-getSuperProperties|)
  (property &optional reasoner) 
  ;; Set<Set<OWLObjectProperty>> bzw. Set<Set<OWLDataProperty>> 
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-role-parents property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-getSubProperties|)
  (property &optional reasoner)
  ;; Set<Set<OWLObjectProperty>> bzw. Set<Set<OWLDataProperty>> 
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-role-children property (owlapi-tbox *cur-reasoner*)))
    
      (return-by-policy))))

(owlapi-defun (|OWLAPI-getAncestorProperties|)
  (property &optional reasoner (remove-self-p t))
  ;; Set<Set<OWLObjectProperty>> bzw. Set<Set<OWLDataProperty>> 
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-role-ancestors property (owlapi-tbox *cur-reasoner*) remove-self-p))
    
      (return-by-policy))))

  
(owlapi-defun (|OWLAPI-getDescendantProperties|)
  (property &optional reasoner (remove-self-p t))
  ;; Set<Set<OWLObjectProperty>> bzw. Set<Set<OWLDataProperty>> 
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-role-descendants property (owlapi-tbox *cur-reasoner*) remove-self-p))
    
      (return-by-policy))))

(owlapi-defun (|OWLAPI-getEquivalentProperties|)
  (property &optional reasoner (remove-self-p t))
  ;; Set<OWLObjectProperty> bzw. Set<OWLDataProperty> 
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-equivalent-roles property (owlapi-tbox *cur-reasoner*) remove-self-p))

      (return-by-policy))))
 

(owlapi-defun (|OWLAPI-getDomains|)
  (property &optional reasoner (owlapi-hacking-mode 2))
  ;; Set<Set<OWLDescription>> 
  (with-reasoner (reasoner)
    (let* ((property (find-owl-property property)))
      
      (setf (last-answer *cur-reasoner*) 
            (reasoner-get-role-domain property (owlapi-tbox *cur-reasoner*) owlapi-hacking-mode))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-getRanges|)
  (property &optional reasoner (owlapi-hacking-mode 2))
  ;; Set<OWLDescription> 
  ;; ist das falsch? 
  ;; geaendert zu  Set<Set<OWLDescription>> 

  (with-reasoner (reasoner)
    (let* ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-get-role-range property (owlapi-tbox *cur-reasoner*) owlapi-hacking-mode))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-isFunctional|)
  (property &optional reasoner)
  ;; boolean

  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-role-functional-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

;;;
;;; 
;;; 

(owlapi-defun (|OWLAPI-getInverseProperties|)
  (property &optional reasoner)
  ;; Set<Set<OWLObjectProperty>> bzw. Set<Set<OWLDataProperty>> 
  
  (with-reasoner (reasoner)
    (let* ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*) 
            (reasoner-inverse-roles property (owlapi-tbox *cur-reasoner*)))
    
      (return-by-policy))))

(owlapi-defun (|OWLAPI-isInverseFunctional|)
  (property &optional reasoner)
  ;; boolean

  (with-reasoner (reasoner)
    (let* ((property (find-owl-property property)))
      
      (setf (last-answer *cur-reasoner*)
            (reasoner-role-inverse-functional-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-isSymmetric|)
  (property &optional reasoner)
  ;; boolean

  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-role-symmetric-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-isTransitive|)
  (property &optional reasoner)
  ;; boolean

  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-role-transitive-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))


(owlapi-defun (|OWLAPI-isReflexive|)
  (property &optional reasoner)
  ;; boolean

  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-role-reflexive-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-isIrreflexive|)
  (property &optional reasoner)
  (declare (ignorable property))
  ;; boolean
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-role-irreflexive-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))


(owlapi-defun (|OWLAPI-isAsymmetric|)
  (property &optional reasoner)
  (declare (ignorable property))
  ;; boolean
  (with-reasoner (reasoner)
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-role-asymmetric-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))

;;;
;;; OWLReasonerBase
;;;

(defun clear-ontologies ()
  (clrhash (ontologies *cur-reasoner*)))


(defpersistentclass ontology ()
  ((name :accessor name :initform (gensym) :initarg :name)
   (all-names :accessor all-names :initform nil :initarg :all-names)
   
   (axioms :accessor axioms :initform nil)
   (ontology-prefixes :accessor ontology-prefixes :initform nil)

   (loaded-p :accessor ont-loaded-p :initform nil)
   (secondary-p :accessor secondary-p :initform nil :initarg :secondary-p)

   ;;; batch mode

   (axioms-to-add-or-remove :accessor axioms-to-add-or-remove :initform nil)

   ;;;
   ;;;
   ;;;

   (referenced-concepts :accessor referenced-concepts :initform (mht))
   (referenced-object-properties :accessor referenced-object-properties :initform (mht))
   (referenced-annotation-properties :accessor referenced-annotation-properties :initform (mht))
   (referenced-data-properties :accessor referenced-data-properties :initform (mht))
   (referenced-individuals :accessor referenced-individuals :initform (mht))
   (referenced-datatypes :accessor referenced-datatypes :initform (mht))
   
   ;;;
   ;;;
   ;;; 

   (declared-concepts :accessor declared-concepts :initform (mht))
   (declared-object-properties :accessor declared-object-properties :initform (mht))
   (declared-data-properties :accessor declared-data-properties :initform (mht))
   (declared-annotation-properties :accessor declared-annotation-properties :initform (mht))
   (declared-individuals :accessor declared-individuals :initform (mht))
   (declared-datatypes :accessor declared-datatypes :initform (mht))))

;;;
;;;
;;;

(defun register-referenced-concept (c ont)
  (when (register-referenced-entities-p *cur-reasoner*)
    (let ((ont1 (find-owl-ontology ont)))
      (labels ((do-it (c) 
                 (cond ((symbolp c)
                        (setf (gethash c (referenced-concepts ont1)) t))
                       ((consp c)

                        (pushnew (first c) (used-constructors *cur-reasoner*))

                        (ecase (first c)
                          (not (do-it (second c)))
                          (one-of (mapc #'(lambda (x) 
                                            (register-referenced-individual x ont))
                                        (rest c)))
                          ((and or) (mapc #'do-it (rest c)))
                          ((some all) 
                           (register-referenced-object-property (second c) ont)
                           (do-it (third c)))
                          ((at-least at-most exactly) 
                           (register-referenced-object-property (third c) ont)
                           (when (fourth c)
                             (do-it (fourth c))))
                          (d-filler
                           (register-referenced-data-property (second c) ont))
                          ((d-complement d-and d-or)
                           (register-referenced-datarange c ont))
                          ((d-restriction d-base-type d-possible-values)
                           (register-referenced-datarange c ont))
                          ((d-some d-all)
                           (register-referenced-data-property (second c) ont)
                           (do-it (third c)))
                          ((d-at-least d-at-most d-exactly) 
                           (register-referenced-data-property (third c) ont)
                           (register-referenced-datarange (fourth c) ont))
                          (has-value 
                           (register-referenced-object-property (second c) ont))
                          (self-reference
                           (register-referenced-object-property (second c) ont))
                          ((a an no
                              min max divisible not-divisible
                              string=  string<> boolean= boolean<>
                              > >= < <= <> = 
                              equal unequal)))))))
        (do-it c)))))
                        

(defun register-referenced-individual (i ont)
  (when (register-referenced-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash i (referenced-individuals ont)) t))))

(defun register-referenced-object-property (p ont)
  (if (consp p)
      (register-referenced-object-property (second p)
                                           (owlapi-tbox *cur-reasoner*))
    (progn 
      (reasoner-ensure-role p (owlapi-tbox *cur-reasoner*))
      (when (register-referenced-entities-p *cur-reasoner*)
        (let ((ont (find-owl-ontology ont)))
          (setf (gethash p (referenced-object-properties ont)) t))))))

(defun register-referenced-annotation-property (p ont)
  (reasoner-ensure-role p (owlapi-tbox *cur-reasoner*))
  (when (register-referenced-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash p (referenced-annotation-properties ont)) t))))

(defun register-referenced-data-property (p ont)
  (reasoner-ensure-role p (owlapi-tbox *cur-reasoner*))
  (when (register-referenced-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash p (referenced-data-properties ont)) t))))

(defun register-referenced-datatype (d ont)
  (when (register-referenced-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash d (referenced-datatypes ont)) t))))

(defun register-referenced-datarange (dr ont)
  (when (register-referenced-entities-p *cur-reasoner*)
    (let ((ont1 (find-owl-ontology ont)))
      (labels ((do-it (c) 
                 (cond ((symbolp c)
                        (setf 
                         (gethash c (referenced-datatypes ont1)) 
                         t))

                       ((consp c)
                       
                        (pushnew (first c) (used-constructors *cur-reasoner*))
         
                        (ecase (first c)
         
                          (d-base-type
                           (do-it (second c)))
                        
                          (d-complement
                           (do-it (second c)))
           
                          ((d-and d-or)
                           (mapc #'do-it (rest c)))

                          (d-possible-values 

                           )

                          (d-facet 
                           
                           )

                          (d-datarange
                           (do-it (second c)))

                          (d-restriction
                         ; base type
                           (when (second c)
                             (do-it (second c)))))))))
        
        (do-it dr)))))

;;;
;;;
;;;

(defun register-declared-concept (c ont) 
  (when (register-declared-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash c (declared-concepts ont)) t))))

(defun register-declared-individual (i ont)
  (when (register-declared-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash i (declared-individuals ont)) t))))

(defun register-declared-object-property (p ont) 
  (when (register-declared-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash p (declared-object-properties ont)) t))))

(defun register-declared-data-property (p ont)
  (when (register-declared-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash p (declared-data-properties ont)) t))))

(defun register-declared-annotation-property (p ont)
  (when (register-declared-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash p (declared-annotation-properties ont)) t))))

(defun register-declared-datatype (d ont)
  (when (register-declared-entities-p *cur-reasoner*)
    (let ((ont (find-owl-ontology ont)))
      (setf (gethash d (declared-datatypes ont)) t))))

;;;
;;;
;;;  

(defmethod print-object ((ont ontology) stream)
  (format stream "#<Ontology ~A with ~A axiom>" 
          (name ont) (length (axioms ont)))) 

(defun find-owl-ontology (name &optional (error-p t))
  (etypecase name
    (string
     (or (gethash name (ontologies *cur-reasoner*))
         (when error-p (owlapi-runtime-error "Can't find ontology ~A" name))))
    (symbol 
     (or (gethash (find-owl-entity name) (ontologies *cur-reasoner*))
         (when error-p (owlapi-runtime-error "Can't find ontology ~A" name))))
    (ontology name)))

;;;
;;;
;;; 

(defmethod load-ontology ((ont ontology))
  
  #| 

  (add-concept-axiom (owlapi-tbox *cur-reasoner*)
                     'top +owlapi-owl-thing+)

  (add-concept-axiom (owlapi-tbox *cur-reasoner*)
                     'bottom +owlapi-owl-nothing+)
  |# 
  
  (load-axioms (axioms ont) :ontology ont)

  (setf (ont-loaded-p ont) t))

(defmethod unload-ontology ((ont ontology))
  (unload-axioms (reverse (axioms ont)) :ontology ont)
  (setf (ont-loaded-p ont) nil))

;;;
;;;
;;; 

(defmethod dispose-ontology ((ont ontology) &optional dispose-axioms-p)
  (dolist (name (all-names ont))
    (remhash name (ontologies *cur-reasoner*)))
  
  (when (eq (first (auto-mode *cur-reasoner*)) ont)
    (setf (auto-mode *cur-reasoner*) nil))
    
  (when dispose-axioms-p
    (dolist (axiom (axioms ont))
      (dispose-axiom axiom))))

;;;
;;;
;;;


(owlapi-defun (|OWLAPI-describeReasoner|)
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*)
          `((:reasoner-name ,(owlapi-reasoner-name *cur-reasoner*))
            #+:racer-server
            (:uses-simplified-protocol ,(or user:*one-simple-output*
                                            (simple-output *cur-reasoner*)))
            (:load-into-tbox ,(owlapi-tbox *cur-reasoner*))
            (:load-into-abox ,(owlapi-abox *cur-reasoner*))
            (:maintain-axiom-objects ,(not (dont-keep-axioms-p *cur-reasoner*)))
            (:ignore-annotations ,(ignore-annotations-p *cur-reasoner*))
            (:ignore-declarations ,(ignore-declarations-p *cur-reasoner*))
            (:auto-mode
             ,(case (second (auto-mode *cur-reasoner*))
                (:add `(:auto-add-axioms-to-ontology-no-change-objects
                        ,(name (first (auto-mode *cur-reasoner*)))))
                (:batch-add `(:batch-add-axioms-to-ontology-no-change-objects
                              ,(name (first (auto-mode *cur-reasoner*)))))
                (:remove `(:auto-remove-axioms-from-ontology-no-change-objects
                           ,(name (first (auto-mode *cur-reasoner*)))))
                (:batch-remove `(:batch-remove-axioms-from-ontology-no-change-objects
                                 ,(name (first (auto-mode *cur-reasoner*)))))))
            (:optimized-incremental-abox-updates-enabled ,(incremental-updates *cur-reasoner*))
            (:auto-apply-changes-if-change-objects-used ,(auto-apply *cur-reasoner*))                        
            (:ontologies ,(get-ontology-names *cur-reasoner*))
            (:no-of-all-axioms ,(hash-table-count (axioms *cur-reasoner*)))
            (:no-of-remaining-changes-for-all-ontologies ,(length (changes *cur-reasoner*)))
            (:registers-referenced-entities ,(register-referenced-entities-p *cur-reasoner*))
            (:registers-declared-entities ,(register-declared-entities-p *cur-reasoner*))
            ))
            
    (return-by-policy)))


(owlapi-defun (|OWLAPI-describeReasoners|)
  ()

  (loop as reasoner being the hash-value of *reasoners* 
        collect (|OWLAPI-describeReasoner| reasoner)))              

(owlapi-defun (|OWLAPI-describeOntology|)
  (ontology &optional reasoner) 

  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ontology)))
      (setf (last-answer *cur-reasoner*)
            `((:ontology-names ,(all-names ont))
              (:no-of-axioms ,(length (axioms ont)))
              (:loaded ,(ont-loaded-p ont))
              (:no-of-remaining-changes ,(length (remove-if-not #'(lambda (x) 
                                                                    (eq (change-ontology x) ont))
                                                                (changes *cur-reasoner*))))
              (:in-reasoner ,(progn 
                               (|OWLAPI-describeReasoner| *cur-reasoner*)
                               (last-answer *cur-reasoner*)))))

      (return-by-policy))))

(owlapi-defun (|OWLAPI-describeOntologies|)
  (&optional reasoner) 

  (with-reasoner (reasoner)
    (mapcar #'(lambda (ont) 
                (|OWLAPI-describeOntology| ont reasoner))
            (get-ontologies *cur-reasoner*))))
              
;;;
;;;
;;;


(owlapi-defun (|OWLAPI-newOntology|)
  (name &optional reasoner secondary-p)
 (with-reasoner (reasoner)
    (let ((ont (make-instance 'ontology :name name :all-names (list name) :secondary-p secondary-p)))

      (setf (gethash name (ontologies *cur-reasoner*)) ont)

      (setf (last-answer *cur-reasoner*) name)

      (return-by-policy))))

(defun get-temp-ontology-name ()
  (gensym))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-mergeOntologies|)
  (ont1 ont2 &optional reasoner)

  (with-reasoner (reasoner)
    (let ((ont1 (find-owl-ontology ont1))
          (ont2 (find-owl-ontology ont2))
          (axioms (get-axioms-in ont2))
          (auto-mode (auto-mode *cur-reasoner*)))

      (unwind-protect 
          (progn 
            (|OWLAPI-autoBatchRemoveAxiomsFrom| ont2)
      
            (dolist (ax axioms)
              (remove-axiom ont2 ax))

            (|OWLAPI-batchSynchronize| ont2)
      
            (dolist (ax axioms)
              (add-axiom ont1 ax))

            (|OWLAPI-batchSynchronize| ont1)

            (setf (all-names ont1)
                  (remove-duplicates
                   (append (all-names ont1)
                           (all-names ont2))))

            (dispose-ontology ont2)
           
            (dolist (name (all-names ont1))
              (setf (gethash name (ontologies *cur-reasoner*)) ont1))
        
            ont1)

        (setf (auto-mode *cur-reasoner*) auto-mode)))))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-disposeOntology|)
  (ont-name &optional reasoner dispose-axioms-p)  

  (with-reasoner (reasoner)

    (let ((ont (find-owl-ontology ont-name)))
      (unload-ontology ont)
  
      (dispose-ontology ont dispose-axioms-p))

    (clear-last-answer)
  
    (return-by-policy)))

(owlapi-defun (|OWLAPI-loadOntology|) 
  (ontology &optional reasoner) 

  (with-reasoner (reasoner)
    (load-ontology (find-owl-ontology ontology))

    (clear-last-answer)

    (return-by-policy)))

(owlapi-defun (|OWLAPI-getOntologies|)
  (&optional reasoner)
  
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) 
          (get-ontology-names *cur-reasoner*))

    (return-by-policy)))

(owlapi-defun (|OWLAPI-getLoadedOntologies|) 
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) 
          (mapcar #'name
                  (remove-if-not #'ont-loaded-p 
                                 (get-ontologies *cur-reasoner*))))

    (return-by-policy)))


(owlapi-defun (|OWLAPI-contains|) 
  (ont-name &optional reasoner)

  (with-reasoner (reasoner)

    (setf (last-answer *cur-reasoner*)
          (when (find-owl-ontology ont-name)
            t))

    (return-by-policy)))

;;;
;;;
;;;

(owlapi-defun (|OWLAPI-getAllOntologies|)
  ()
  
  (setf (last-answer *cur-reasoner*) 
        (get-all-ontologies))
 
  (return-by-policy))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-loadOntologies|) 
  (ontologies &optional reasoner)
  
  (with-reasoner (reasoner)
    (mapc #'|OWLAPI-loadOntology| ontologies)

    (clear-last-answer)

    (return-by-policy)))


(owlapi-defun (|OWLAPI-disposeOntologies|) 
  (ontologies &optional reasoner)
  
  (with-reasoner (reasoner)
    (mapc #'|OWLAPI-disposeOntology| ontologies)

    (clear-last-answer)

    (return-by-policy)))

(owlapi-defun (|OWLAPI-reloadLoadedOntologies|)
  (&optional reasoner)

  (with-reasoner (reasoner) 
    (let ((onts (|OWLAPI-getLoadedOntologies|)))
      (dolist (ont onts)
        (|OWLAPI-loadOntology| ont)))
	    
    (clear-last-answer)
	    
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-setAutoDeclareDataProperties|)
  (val &optional reasoner)
  (with-reasoner (reasoner)
    (setf (auto-declare-datatype-properties-p *cur-reasoner*) val)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-getAutoDeclareDataProperties|)
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*)
          (auto-declare-datatype-properties-p *cur-reasoner*))
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-getAutoOntology|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (clear-last-answer)
    (when (auto-mode *cur-reasoner*)
      (setf (last-answer *cur-reasoner*)
            (name (first (auto-mode *cur-reasoner*)))))
    
    (return-by-policy)))
    
(owlapi-defun (|OWLAPI-autoAddAxiomsTo|)
  (ontology &optional reasoner)

  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ontology)))
      (setf (auto-mode *cur-reasoner*) (list ont :add))
      (clear-last-answer)
      (return-by-policy))))

(owlapi-defun (|OWLAPI-autoBatchAddAxiomsTo|)
  (ontology &optional reasoner)

  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ontology)))
      (setf (auto-mode *cur-reasoner*) (list ont :batch-add))
      (clear-last-answer)
      (return-by-policy))))

    
(owlapi-defun (|OWLAPI-autoRemoveAxiomsFrom|)
  (ontology &optional reasoner)

  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ontology)))
      (setf (auto-mode *cur-reasoner*) (list ont :remove))
      (clear-last-answer)
      (return-by-policy))))

(owlapi-defun (|OWLAPI-autoBatchRemoveAxiomsFrom|)
  (ontology &optional reasoner)

  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ontology)))
      (setf (auto-mode *cur-reasoner*) (list ont :batch-remove))
      (clear-last-answer)
      (return-by-policy))))

    
(owlapi-defun (|OWLAPI-disableAutoMode|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (auto-mode *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-enableLookupMode|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (lookup-mode *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-disableLookupMode|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (lookup-mode *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;;

(owlapi-defun (|OWLAPI-enableTransientAxiomMode|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (transient-mode *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))


(owlapi-defun (|OWLAPI-disableTransientAxiomMode|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (transient-mode *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;;

(defmacro with-lookup-mode
          ((&optional reasoner) &body body)
  `(unwind-protect
       (progn 
         (|OWLAPI-enableLookupMode| ,reasoner)
         ,@body)
     (|OWLAPI-disableLookupMode| ,reasoner)))

(defmacro with-transient-axiom-mode 
          ((&optional reasoner) &body body)
  `(unwind-protect
       (progn 
         (|OWLAPI-enableTransientAxiomMode| ,reasoner)
         ,@body)
     (|OWLAPI-disableTransientAxiomMode| ,reasoner)))
    
;;;
;;;
;;;

(owlapi-defun (|OWLAPI-batchSynchronize|)
  (ontology &optional reasoner)

  (with-reasoner (reasoner)
    (let* ((ont (find-owl-ontology ontology))
           (commands 
            (nreverse (axioms-to-add-or-remove ont)))
           
           (rem-start
            (position :remove commands :key #'first))
           (block-remove-p 
            (and rem-start
                 (every #'(lambda (x) 
                            (eq (first x) :remove))
                        (subseq commands rem-start))))
           (mode (auto-mode *cur-reasoner*)))

      (setf (auto-mode *cur-reasoner*)
            (list (first mode)
                             (ecase (second mode)
                               (:batch-add :add)
                               (:batch-remove :remove))))

      (loop as (command ax) in commands do
            (ecase command
              (:add 
               (if (dont-keep-axioms-p *cur-reasoner*)
                   (load-axiom ax :ontology ont)
                 (add-axiom ont ax)))
              (:remove 
               (unless block-remove-p
                 (remove-axiom ont ax)))))


      (when block-remove-p
        (remove-axioms ont
                       (loop as (command ax) in commands 
                             when (eq command :remove) 
                             collect ax)))

      (setf (axioms-to-add-or-remove ont) nil)

      (setf (auto-mode *cur-reasoner*) mode)

      (clear-last-answer)
      (return-by-policy))))

;;;
;;; 
;;;

(owlapi-defun (|OWLAPI-enableMemorySavingMode|)
  (ontology &optional reasoner)

  (|OWLAPI-autoAddAxiomsTo| ontology reasoner)
  
  (with-reasoner (reasoner)
    (setf (dont-keep-axioms-p *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-disableMemorySavingMode|)
  (reasoner)

  (|OWLAPI-disableAutoMode| reasoner)

  (with-reasoner (reasoner)
    (setf (dont-keep-axioms-p *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-ignoreAnnotations|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (ignore-annotations-p *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-keepAnnotations|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (ignore-annotations-p *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-ignoreDeclarations|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (ignore-declarations-p *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-considerDeclarations|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (ignore-declarations-p *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;; 


(owlapi-defun (|OWLAPI-isClassified|)
  (&optional reasoner)

  (with-reasoner (reasoner)

    (setf (last-answer *cur-reasoner*)
          (reasoner-tbox-classified-p (owlapi-tbox *cur-reasoner*)))
    
    (return-by-policy)))

(owlapi-defun (|OWLAPI-classify|)
  (&optional reasoner (check-abox-consistency-p t))

  (with-reasoner (reasoner)

    (reasoner-classify (owlapi-tbox *cur-reasoner*))

    (when check-abox-consistency-p
      (reasoner-abox-consistent-p (owlapi-abox *cur-reasoner*)))

    (clear-last-answer)

    (return-check-if-abox-consistent)))


(owlapi-defun (|OWLAPI-isRealised|)
  (&optional reasoner)

  (with-reasoner (reasoner)

    (setf (last-answer *cur-reasoner*)
          (reasoner-abox-realized-p (owlapi-abox *cur-reasoner*)))
    
    (return-by-policy)))

(owlapi-defun (|OWLAPI-realize|)
  (&optional reasoner (check-abox-consistency-p t))

  (with-reasoner (reasoner)

    (reasoner-realize (owlapi-abox *cur-reasoner*))

    (when check-abox-consistency-p
      (reasoner-abox-consistent-p (owlapi-abox *cur-reasoner*)))

    (clear-last-answer)

    (return-check-if-abox-consistent)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-isDefinedClass|) 
  (cls &optional reasoner)

  (with-reasoner (reasoner)
  
    (let ((cls (find-owl-class cls)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-concept-p cls (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))


(owlapi-defun (|OWLAPI-isDefinedObjectProperty|) 
  (property &optional reasoner)

  (with-reasoner (reasoner)
    
    (let ((property (find-owl-property property)))
      
      (setf (last-answer *cur-reasoner*)
            (reasoner-object-property-p property (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))


(owlapi-defun (|OWLAPI-isDefinedDataProperty|) 
  (property &optional reasoner)

  (with-reasoner (reasoner)
  
    (let ((property (find-owl-property property)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-data-property-p property  (owlapi-tbox *cur-reasoner*)))

      (return-by-policy))))


(owlapi-defun (|OWLAPI-isDefinedIndividual|)
  (ind &optional reasoner)

  (with-reasoner (reasoner)
    
    (let ((ind (find-owl-individual ind)))

      (setf (last-answer *cur-reasoner*)
            (reasoner-individual-p ind (owlapi-abox *cur-reasoner*)))

      (return-by-policy))))


;;;
;;;
;;;

(owlapi-defun (|OWLAPI-unloadOntologies|) 
  (ontologies &optional reasoner)

  (with-reasoner (reasoner)
  
    (dolist (ont ontologies)
      (unload-ontology (find-owl-ontology ont)))

    (clear-last-answer)

    (return-by-policy)))

(owlapi-defun (|OWLAPI-unloadOntology|) 
  (ontology &optional reasoner)

  (with-reasoner (reasoner)

    (|OWLAPI-unloadOntologies| (list (find-owl-ontology ontology)))))

(owlapi-defun (|OWLAPI-clearOntologies|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (|OWLAPI-unloadOntologies| (|OWLAPI-getLoadedOntologies|))))


(owlapi-defun (|OWLAPI-dispose|)
  ()

  (let ((reasoners 
         (loop as reasoner being the hash-value of *reasoners*
               collect reasoner)))
    
    (dolist (reasoner reasoners)
      (|OWLAPI-disposeReasoner| reasoner))

    (clear-last-answer)
    (return-by-policy)))
  

;;;
;;; OWLSatisfiabilityChecker
;;;


(owlapi-defun (|OWLAPI-isSatisfiable|)
  (description &optional reasoner)

  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) 
          (reasoner-concept-satisfiable-p (find-owl-class description)
                                          (owlapi-tbox *cur-reasoner*)))

    (return-by-policy)))

;;;
;;; Axioms
;;;

(owlapi-defun (|OWLAPI-getAxiomCounter|)
  (&optional reasoner)

  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) (axiom-counter *cur-reasoner*))
    (return-by-policy)))


(owlapi-defun (|OWLAPI-setAxiomCounter|)
  (n &optional reasoner)

  (with-reasoner (reasoner)
    (setf (axiom-counter *cur-reasoner*) n)
    (clear-last-answer)
    (return-by-policy)))

(defun reset-axiom-counter () 
  (setf (axiom-counter *cur-reasoner*) 0))

(owlapi-defun (|OWLAPI-resetAxiomCounter|)
  (&optional reasoner)
  (with-reasoner (reasoner)
    (reset-axiom-counter)
    (clear-last-answer)
    (return-by-policy)))

(defun clear-axioms ()
  (clrhash (axioms *cur-reasoner*)))

(defun get-axioms (&optional reasoner)
  (with-reasoner (reasoner)
    (loop as axiom being the hash-value of (axioms *cur-reasoner*)
          collect axiom)))

(defun get-axioms-in (ont &optional reasoner)
  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ont)))
      (loop as axiom being the hash-value of (axioms *cur-reasoner*)
            when (member ont (in-ontologies axiom))
            collect axiom))))


(defun get-axioms-of-type-for (axioms type &optional slot value)
  ;;; inefficent! use with care!
  (let* ((axioms
          (if (not (hash-table-p axioms))
              axioms
            (loop as axiom being the hash-value of axioms
                  collect axiom))))

    (remove-if-not #'(lambda (axiom)
                       (and (typep axiom type)
                            (or (not (and slot value))
                                (equalp (slot-value axiom slot) value))))
                   axioms)))

(defaxiom |OWLAxiom| () 
  ((axiom-id :accessor axiom-id :initform nil)
   (told :accessor told :initform nil :initarg :told)
   (is-tbox-axiom-p :accessor is-tbox-axiom-p :initform nil :initarg :is-tbox-axiom-p)
   (annotations :accessor annotations :initform nil :initarg :annotations)
   (loaded-p :accessor loaded-p :initform nil)
   (in-reasoner :accessor in-reasoner :initform nil :initarg :in-reasoner)
   (in-ontologies :accessor in-ontologies :initform nil :initarg :in-ontologies)
   (can-be-unloaded-p :accessor can-be-unloaded-p :initform nil)))

;;;
;;;
;;; 

(defmethod entailed-p ((axiom |OWLAxiom|)) 
  :unknown)  

(defmethod direct-entailed-p ((axiom |OWLAxiom|)) 
  :unknown)  

;;;
;;;
;;;

(defun very-equal-p (c1 c2)
  (or (equalp c1 c2)
      (and (symbolp c1)
           (symbolp c2)
           (string= (symbol-name c1) 
                    (symbol-name c2)))))
    
(defun concepts-equal-p (c1 c2)
  (or (very-equal-p c1 c2)
      (and (symbolp c1)
           (symbolp c2)
           (or (and (member c1 (list 'top +owlapi-owl-thing+))
                    (member c2 (list 'top +owlapi-owl-thing+)))
               (and (member c1 (list 'bottom +owlapi-owl-nothing+))
                    (member c2 (list 'bottom +owlapi-owl-nothing+)))))
      (and (eq (type-of c1) (type-of c2))
           (typecase c1
             (list 
              (let ((op1 (first c1))
                    (op2 (first c2)))
                (and (eq op1 op2)
                     (case op1
                       ((and or d-and d-or d-restriction one-of)
                        (let ((c1 (if (>= (length c1) (length c2))
                                      c1
                                    c2))
                              (c2 (if (>= (length c1) (length c2))
                                      c2
                                    c1)))
                          (every #'(lambda (arg1)
                                     (some #'(lambda (arg2)
                                               (concepts-equal-p arg1 arg2))
                                           (rest c2)))
                                 (rest c1))))
                       (otherwise 
                        (and (= (length c1) (length c2))
                             (every #'(lambda (c1 c2)
                                        (concepts-equal-p c1 c2))
                                    c1 c2)))))))
             (string
              (string-equal c1 c2))
             (otherwise nil)))))


(defmethod slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots (comp (eql :equal)) &key &allow-other-keys)
  (every #'(lambda (x)
             (very-equal-p (slot-value ax1 x)
                           (slot-value ax2 x)))
         slots))

(defmethod slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots (comp (eql :set)) 
                          &key (test #'very-equal-p))                                                           
  (every #'(lambda (x)
             (set-equal (slot-value ax1 x)
                        (slot-value ax2 x)
                        :test test))
         slots))


(defmethod slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots (comp (eql :list)) 
                          &key (test #'very-equal-p))                                                           
  (every #'(lambda (x)
             (every #'(lambda (a b)
                        (funcall test a b))
                    (slot-value ax1 x)
                    (slot-value ax2 x)))
         slots))

(defmethod slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots (comp (eql :concept)) &key &allow-other-keys)
  (every #'(lambda (x)
             (concepts-equal-p (slot-value ax1 x)
                               (slot-value ax2 x)))
         slots))

(defmethod slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots (comp (eql :datarange)) &key &allow-other-keys)
  (every #'(lambda (x)
             (concepts-equal-p (slot-value ax1 x)
                               (slot-value ax2 x)))
         slots))

(defmethod default-slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots)
  (slots-equal-p ax1 ax2 slots :equal))

(defmethod set-slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots)
  (slots-equal-p ax1 ax2 slots :set))

(defmethod list-slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots)
  (slots-equal-p ax1 ax2 slots :list))

(defmethod concept-slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots)
  (slots-equal-p ax1 ax2 slots :concept))

(defmethod datarange-slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots)
  (slots-equal-p ax1 ax2 slots :datarange))

(defmethod set-of-concepts-slots-equal-p ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|) slots)
  (slots-equal-p ax1 ax2 slots :set :test #'concepts-equal-p))

;;;
;;;
;;;

(defgeneric axioms-equal-p (ax1 ax2) (:method-combination and))

(defmethod axioms-equal-p and ((ax1 |OWLAxiom|) (ax2 |OWLAxiom|))
  (eq (type-of ax1) (type-of ax2)))

#+:racer-server
(defmethod warning-ignored-axiom ((ax |OWLAxiom|))
  (when *tbox-verbose*
    (let ((ts::*really-warn-p* t))
      (owlapi-warning "Ignoring axiom ~A of type ~A"
                      ax
                      (type-of ax)))))

#+:racer-server
(defmethod warning-redundant-axiom ((ax list))
  (when *tbox-verbose*
    (let ((ts::*really-warn-p* t))
      (owlapi-warning "Returning existing structurally equivalent axiom for constructor call ~A"
                      ax))))


#-:racer-server
(defmethod warning-ignored-axiom ((ax |OWLAxiom|))
  (owlapi-warning "Ignoring axiom ~A of type ~A"
                  ax
                  (type-of ax)))

#-:racer-server
(defmethod warning-redundant-axiom ((ax list))
  (owlapi-warning "Returning existing structurally equivalent axiom for constructor call ~A"
                  ax))

;;;
;;; 
;;; 

(defmethod add-axiom ((ont ontology) (ax |OWLAxiom|))
  (unless (member ont (in-ontologies ax))
    (cond ((member (second (auto-mode *cur-reasoner*)) '(:batch-add))
           (push (list :add ax)
                 (axioms-to-add-or-remove ont)))
          (t
           (push ax (axioms ont))
           (push ont (in-ontologies ax))
           (when (ont-loaded-p ont)
             (load-axiom ax :ontology ont))))))

(defmethod remove-axiom ((ont ontology) (ax |OWLAxiom|))
  (cond ((member (second (auto-mode *cur-reasoner*)) '(:batch-remove))
         (push (list :remove ax)
               (axioms-to-add-or-remove ont)))

        (t 
         (setf (axioms ont) 
               (delete ax (axioms ont)))
         (setf (in-ontologies ax)
               (delete ont (in-ontologies ax)))
  
         (unload-axiom ax :ontology ont))))

(defmethod remove-axioms ((ont ontology) (axioms list))
  (setf (axioms ont) 
        (set-difference (axioms ont) axioms))
  (dolist (ax axioms) 
    (setf (in-ontologies ax)
          (delete ont (in-ontologies ax))))
  
  (if (and (incremental-updates *cur-reasoner*)
           (every #'can-be-unloaded-p axioms))
      
      (dolist (axiom axioms)
        (unload-axiom axiom :ontology ont))

    (unload-axioms axioms :ontology ont)))

;;;
;;; Load 
;;; 

(defun load-axioms (axioms &key ontology)
  (dolist (axiom axioms)
    (load-axiom axiom :ontology ontology)))

(defmethod load-axiom :around ((axiom |OWLAxiom|) &key ontology)
  (declare (ignorable ontology))
  (unless (loaded-p axiom)
    (call-next-method)
    (setf (loaded-p axiom) t)))

(defmethod load-axiom ((axiom |OWLAxiom|) &key ontology)
  (declare (ignorable ontology))
  (warning-ignored-axiom axiom))

;;;
;;; Unload 
;;; 

(defun unload-axioms (axioms &key ontology)
  (cond ((and (incremental-updates *cur-reasoner*)
              (every #'(lambda (x) 
                         (or (can-be-unloaded-p x)
                             (typep x '|OWLDeclarationAxiom|)
                             (typep x '|OWLImportsDeclarationAxiom|)
                             (typep x '|OWLOntologyVersionDeclarationAxiom|)
                             (typep x '|OWLPrefixDeclarationAxiom|)))
                     axioms))

         (dolist (axiom axioms)
           ;;; fuehrt dann zu unload-axiom1:
           (unless (or (typep axiom '|OWLDeclarationAxiom|)
                       (typep axiom '|OWLImportsDeclarationAxiom|)
                       (typep axiom '|OWLOntologyVersionDeclarationAxiom|)
                       (typep axiom '|OWLPrefixDeclarationAxiom|))
             (unload-axiom axiom :ontology ontology))))
      
        (t

         (let ((ensure-reload-of-tbox-axioms-p nil))

           (if (not (some #'is-tbox-axiom-p axioms))
               (reasoner-clear-abox 
                (owlapi-tbox *cur-reasoner*) 
                (owlapi-abox *cur-reasoner*))
             (progn 
               (reasoner-clear-tbox-and-abox 
                (owlapi-tbox *cur-reasoner*) 
                (owlapi-abox *cur-reasoner*))
               (setf ensure-reload-of-tbox-axioms-p t)))

           (dolist (ax axioms)
             (setf (loaded-p ax) nil))

           (dolist (ont (get-ontologies *cur-reasoner*))
             
             (let ((old-axioms
                    (axioms ont)))
               
                (setf (axioms ont) 
                      (set-difference old-axioms axioms))

                (when ensure-reload-of-tbox-axioms-p 
                  (dolist (axiom old-axioms)
                    (when (is-tbox-axiom-p axiom)
                      (setf (loaded-p axiom) nil))))

                (|OWLAPI-loadOntology| ont)

                (setf (axioms ont) old-axioms)))))))

(defmethod unload-axiom :around ((axiom |OWLAxiom|) &key ontology)
  (declare (ignorable ontology))
  (when (loaded-p axiom)
    (call-next-method)
    (setf (loaded-p axiom) nil)))

(defmethod unload-axiom ((axiom |OWLAxiom|) &key ontology)
  (if (and (incremental-updates *cur-reasoner*)
           (can-be-unloaded-p axiom))
      (unload-axiom1 axiom :ontology ontology)
    (unload-axioms (list axiom) :ontology ontology)))

(defmethod unload-axiom1 ((axiom |OWLAxiom|) &key ontology)
  (declare (ignorable ontology)))

;;;
;;; 
;;; 

(owlapi-defun (|OWLAPI-loadAxiom|) (ont axiom &optional reasoner)
  (with-reasoner (reasoner)
    (let ((ax (find-owl-axiom axiom ont)))
      (load-axiom ax :ontology ont))))

(owlapi-defun (|OWLAPI-loadAxioms|) (ont axioms &optional reasoner)
  (with-reasoner (reasoner)
    (load-axioms (mapcar #'(lambda (axiom) (find-owl-axiom axiom ont)) axioms)
                 :ontology ont)))

(owlapi-defun (|OWLAPI-unloadAxiom|) (ont axiom &optional reasoner)
  (with-reasoner (reasoner)
    (let ((ax (find-owl-axiom axiom ont)))
      (unload-axiom ax :ontology ont))))

(owlapi-defun (|OWLAPI-unloadAxioms|) (ont axioms &optional reasoner)
  (with-reasoner (reasoner)
    (unload-axioms (mapcar #'(lambda (axiom) (find-owl-axiom axiom ont)) axioms)
                   :ontology ont)))

;;;
;;; 
;;; 

(defmethod initialize-instance :after ((axiom |OWLAxiom|) &rest initargs)
  (declare (ignorable initargs))
  (initialize-axiom axiom t))

(defmethod initialize-axiom ((axiom |OWLAxiom|) new-p)
  
  (cond ((auto-mode *cur-reasoner*)  

         (let ((ont (first (auto-mode *cur-reasoner*)))
               (mode (second (auto-mode *cur-reasoner*))))
      
           (ecase mode
             ((:add :batch-add)
	      
	      (cond ((dont-keep-axioms-p *cur-reasoner*)
		     
		     (case mode 
                       (:add 
                        (load-axiom axiom :ontology ont))
                       (:batch-add 
                        (add-axiom ont axiom))) 

		     (setf (axiom-id axiom) :void))
		    
		    (t 
		     
		     (when new-p 
		       (setf (axiom-id axiom) 
                             (or (next-axiom-use-id *cur-reasoner*)
                                 (incf (axiom-counter *cur-reasoner*)))))

                     (setf (in-reasoner axiom) *cur-reasoner*)
                     (setf (next-axiom-use-id *cur-reasoner*) nil)
		     (setf (gethash (axiom-id axiom) (axioms *cur-reasoner*)) axiom)
                     
                     (unless (first (last (told axiom)))
                       (setf (told axiom) (append (butlast (told axiom))
                                                  (list (owlapi-reasoner-name *cur-reasoner*)))))
                     
                     (if (gethash (told axiom) (axioms-told *cur-reasoner*))
                         (push axiom (gethash (told axiom) (axioms-told *cur-reasoner*)))
                       (setf (gethash (told axiom) (axioms-told *cur-reasoner*)) (list axiom)))
		     
		     (add-axiom ont axiom))))

             ((:remove :batch-remove)
              
	      (cond ((dont-keep-axioms-p *cur-reasoner*)
		     
		     (owlapi-runtime-error "Memory saving mode for reasoner ~A is enableded, can't remove axiom"
                                           *cur-reasoner*))
		    
		    (t
		     
		     (when new-p 
		       (owlapi-runtime-error "Sorry, internal error occured: ~A" (auto-mode *cur-reasoner*)))

		     (remove-axiom ont axiom)
		       
		     (dolist (change (changes *cur-reasoner*))
		       (when (eq (axiom change) axiom)
			 (setf (changes *cur-reasoner*) (delete change (changes *cur-reasoner*)))))))))))
        
	(new-p
	  
	 (setf (axiom-id axiom) 
               (or (incf (axiom-counter *cur-reasoner*))
                   (next-axiom-use-id *cur-reasoner*)))

         (setf (in-reasoner axiom) *cur-reasoner*)
	 (setf (next-axiom-use-id *cur-reasoner*) nil)	 
         (setf (gethash (axiom-id axiom) (axioms *cur-reasoner*)) axiom)
         
         (unless (first (last (told axiom)))
           (setf (told axiom) (append (butlast (told axiom))
                                      (list (owlapi-reasoner-name *cur-reasoner*)))))         

         (if (gethash (told axiom) (axioms-told *cur-reasoner*))
             (push axiom (gethash (told axiom) (axioms-told *cur-reasoner*)))
           (setf (gethash (told axiom) (axioms-told *cur-reasoner*)) (list axiom))))))


(defun find-owl-axiom (id &optional ont) 
  (typecase id
    (|OWLAxiom| id)
    (integer 
     (or (gethash id (axioms *cur-reasoner*))
         (owlapi-runtime-error "Axiom ~A not found" id)))
    (list
     (find-equal-axiom id ont))
    (string 
     (find-equal-axiom id ont))))

;;;
;;;
;;;

(defmethod find-equal-axiom ((axiom-constructor-call list) &optional ont)
  (let* ((axiom-constructor-call  ;; nil optional reasoner entfernen!
                                  (if (not (first (last axiom-constructor-call)))
                                      (append (butlast axiom-constructor-call)
                                              (list (owlapi-reasoner-name *cur-reasoner*)))
                                    axiom-constructor-call))
         (axioms 
          (gethash axiom-constructor-call (axioms-told *cur-reasoner*)))
         (ont (or (when ont (find-owl-ontology ont))
                  (first (auto-mode *cur-reasoner*)))))
    (loop as axiom2 in axioms
          when (or (not ont) 
                   (member ont (in-ontologies axiom2)))
          return axiom2)))

(defmethod find-equal-axiom ((axiom-constructor-call string) &optional ont)
  (let ((axioms 
         (gethash axiom-constructor-call (axioms-told *cur-reasoner*)))
        (ont (or (when ont (find-owl-ontology ont))
                 (first (auto-mode *cur-reasoner*)))))
    (loop as axiom2 in axioms
          when (or (not ont) 
                   (member ont (in-ontologies axiom2)))
          return axiom2)))

(defmethod find-equal-axiom ((axiom |OWLAxiom|) &optional ont)
  (let* ((axiom-constructor-call (told axiom))
	 (axiom-constructor-call 
	  (if (not (first (last axiom-constructor-call)))
              (append (butlast axiom-constructor-call)
                      (list (owlapi-reasoner-name *cur-reasoner*)))
	    axiom-constructor-call))
         (axioms 
          (gethash axiom-constructor-call (axioms-told *cur-reasoner*)))
         (ont (or (when ont (find-owl-ontology ont))
                  (first (auto-mode *cur-reasoner*)))))
    (loop as axiom2 in axioms
          when (or (not ont) 
                   (member ont (in-ontologies axiom2)))
          return axiom2)))

;;;
;;;
;;;

(defmethod dispose-axiom ((axiom |OWLAxiom|))
  (dolist (ont (in-ontologies axiom))
    (remove-axiom ont axiom))
  (remhash (axiom-id axiom) (axioms *cur-reasoner*))
  (setf (gethash (told axiom) (axioms-told *cur-reasoner*))
        (delete (told axiom) 
                (gethash (told axiom) (axioms-told *cur-reasoner*))
                :test #'equal)))

(defmacro return-policy-new-axiom (axiom)
  (let ((ax (gensym))
        (ax2 (gensym))
        
        (told (gensym)))

    (unless (eq (first axiom) 'make-instance)
      (error "bad argument ~A to return-policy-new-axiom" axiom))

    `(let* ((,ax (list ,@(rest axiom)))
            (,told 
             ,(second (member :told (rest axiom))))
            (,ax (let ((ax (find-equal-axiom ,told)))
		   (cond (ax
			  (initialize-axiom ax nil)
			  ax)

                         ((lookup-mode  *cur-reasoner*)
                          :not-found)

			 (t
			  (if (member (second (auto-mode *cur-reasoner*))
				      '(:dummy :remove :batch-remove))
			      (owlapi-runtime-error "Can't find axiom ~S" ,told)
			    
			    ;;;(apply #'make-instance ,ax)
			    
			    (let ((,ax2
                                   (apply (symbol-function 
                                           (quote ,(intern 
						    (format nil 
                                                            #+:mlisp "make-~A"
                                                            #-:mlisp "MAKE-~A"
                                                            (second (second axiom))))))
                                          (rest ,ax))))

                              (unless (transient-mode *cur-reasoner*)
                                (initialize-axiom ,ax2 t))
                              
                              ,ax2)))))))
       
       (cond ((transient-mode *cur-reasoner*)
              ;; get the axiom object
              ,ax)

             ((return-policy *cur-reasoner*) 
              (setf (last-answer *cur-reasoner*)
                    (if (eq ,ax :not-found)
                        :not-found
                      (axiom-id ,ax)))
              (return-by-policy))

             (t ,ax)))))

(owlapi-defun (|OWLAPI-disposeAxiom|) 
  (id-or-constructor &optional reasoner)

  (with-reasoner (reasoner)
    (dispose-axiom (or (find-owl-axiom id-or-constructor)
                       (owlapi-runtime-error "Can't find axiom ~S" id-or-constructor)))
    (clear-last-answer)
    (return-by-policy)))


(owlapi-defun (|OWLAPI-disposeAxioms|) 
  (ids-or-constructors &optional reasoner)
  
  (dolist (axiom ids-or-constructors)
    (|OWLAPI-disposeAxiom| axiom reasoner))
                        
  (clear-last-answer)
  (return-by-policy))

;;;
;;; 
;;; 

(owlapi-defun (|OWLAPI-IDToAxiom|) 
  (id &optional reasoner)
  
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) (told (find-owl-axiom id)))    
    (return-by-policy)))


(owlapi-defun (|OWLAPI-AxiomToID|) 
  (axiom-constructor-call &optional reasoner ont)

  (with-reasoner (reasoner)
    (let ((axiom (find-equal-axiom axiom-constructor-call ont)))

      (setf (last-answer *cur-reasoner*)
            (if axiom 
                (axiom-id axiom)
              :not-found))

      (return-by-policy))))


;;;
;;;
;;;

(owlapi-defun (|OWLAPI-AxiomLoaded?|) 
  (id &optional reasoner)
  
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*) (loaded-p (find-owl-axiom id))) 
    (return-by-policy)))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-getAxioms|) 
  (&optional reasoner with-ids-p with-ont-names-p status)

  (with-reasoner (reasoner)
    (let ((res nil))
      (maphash #'(lambda (key value)
                   (when (=> status 
                             (ecase status
                               (:loaded (loaded-p value))
                               (:unloaded (not (loaded-p value)))))
                     (push 
                      (let ((ax (if with-ids-p 
                                    (list key value)
                                  value)))
                        (if with-ont-names-p 
                            (list (mapcar #'name (in-ontologies value)) ax)
                          ax))
                      res)))
               (axioms *cur-reasoner*))
      (setf (last-answer *cur-reasoner*) res)
      (return-by-policy))))

(owlapi-defun (|OWLAPI-getAxiomsPerOntology|) 
  (&optional reasoner)
  (|OWLAPI-getAxioms| reasoner t t))

(owlapi-defun (|OWLAPI-getAxiomsIn|) 
  (ont &optional reasoner with-ids-p status) 

  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ont)))
      (setf (last-answer *cur-reasoner*)
            (remove nil
                    (mapcar #'(lambda (value)
                                (when (=> status 
                                          (ecase status
                                            (:loaded (loaded-p value))
                                            (:unloaded (not (loaded-p value)))))
                                  (if with-ids-p 
                                      (list (axiom-id value) value)
                                    value)))
                            (axioms ont))))
      (return-by-policy))))


(owlapi-defun (|OWLAPI-getAxiomsOfType|) 
  (type-or-types &optional reasoner with-ids-p with-ont-names-p status)

  (with-reasoner (reasoner)
    (let ((res nil)
	  (type `(or ,@(ensure-list type-or-types))))

      (maphash #'(lambda (key value)
                   (when (and (=> status 
                                  (ecase status
                                    (:loaded (loaded-p value))
                                    (:unloaded (not (loaded-p value)))))
                              (=> type-or-types (typep value type)))
                     (push 
		      (let ((ax
			     (if with-ids-p 
				 (list key value)
			       value)))
			(if with-ont-names-p 
			    (list (mapcar #'name (in-ontologies value)) ax)
			  ax))
                      res)))
               (axioms *cur-reasoner*))
      (setf (last-answer *cur-reasoner*) res)
      (return-by-policy))))
  

(owlapi-defun (|OWLAPI-getAxiomsOfTypeIn|) 
  (type-or-types ont &optional reasoner with-ids-p status) 
  (with-reasoner (reasoner)
    (let ((ont (find-owl-ontology ont))
	  (type `(or ,@(ensure-list type-or-types))))
      (setf (last-answer *cur-reasoner*)
            (remove nil
                    (mapcar #'(lambda (value)
                                (when (and (=> status 
                                               (ecase status
                                                 (:loaded (loaded-p value))
                                                 (:unloaded (not (loaded-p value)))))
                                           (=> type-or-types (typep value type)))
                                  (if with-ids-p 
                                      (list (axiom-id value) value)
                                    value)))
                            (axioms ont))))
      (return-by-policy))))

;;;
;;;
;;;

(defmethod get-annotation-axioms-for-axiom ((axiom-id t))
 (let ((ax (find-owl-axiom axiom-id)))
   (if (not ax)
       :not-found
   (get-annotation-axioms-for-axiom ax))))

(defmethod get-annotation-axioms-for-axiom ((axiom |OWLAxiom|))
  (annotations axiom))

(owlapi-defun (|OWLAPI-getAnnotationAxiomsForAxiom|) 
  (axiom-id &optional reasoner)

  (with-reasoner (reasoner)

    (setf (last-answer *cur-reasoner*)

          (get-annotation-axioms-for-axiom axiom-id))

    (return-by-policy)))

;;;
;;;
;;;

(defun expand-prefix (prefix)
  (or (gethash (when prefix ; dont make nil a keyword
                 (to-keyword (ensure-ends-with-colon prefix)))
               (namespace-table *cur-reasoner*))
      (gethash :defaultnamespace
               (namespace-table *cur-reasoner*))
      ;;; sollte ich das machen? 
      (reasoner-get-default-namespace-prefix (owlapi-tbox *cur-reasoner*))))
       
(defun make-uri (prefix symbol)
  (let ((prefix (expand-prefix prefix)))
    (if prefix
        (intern 
         (if (find #\# (ensure-string prefix))
             (if (find #\# (ensure-string symbol))
                 (format nil "~A~A"
                         prefix
                         (without-starting-\# symbol))
               (format nil "~A~A"
                       prefix
                       symbol))
           (if (find #\# (ensure-string symbol))
               (format nil "~A~A"
                       prefix
                       symbol)
             (format nil "~A~A"
                     prefix
                     symbol))))
      (intern (ensure-string symbol)))))

;;;
;;;
;;;

(owlapi-defun (|OWLAPI-addPrefix|) 
  (prefix namespace &optional reasoner) 

  (with-reasoner (reasoner)
    (let* ((namespace (ensure-string namespace))
           (prefix 
            (when prefix  ; dont make nil a keyword... 
              (unless (keywordp prefix)
                (to-keyword 
                 (ensure-ends-with-colon prefix))))))
      
      (setf (gethash prefix 
                     (namespace-table *cur-reasoner*))
            namespace)
      
      (when (is-default-prefix-p prefix)
        (dolist (prefix +defaultnamespace-prefixes+)
          (setf (gethash prefix 
                         (namespace-table *cur-reasoner*))
                namespace)))

      (reasoner-register-prefix prefix namespace (owlapi-tbox *cur-reasoner*))
    
      (clear-last-answer)

      (return-by-policy))))


(owlapi-defun (|OWLAPI-removePrefix|) 
  (prefix &optional reasoner) 

  (with-reasoner (reasoner)
    (let ((prefix 
           (when prefix
             (to-keyword 
              (ensure-ends-with-colon prefix)))))
      
      (remhash prefix 
               (namespace-table *cur-reasoner*))

      
      (reasoner-remove-prefix prefix (owlapi-tbox *cur-reasoner*))

      (clear-last-answer)

      (return-by-policy))))


(owlapi-defun (|OWLAPI-getPrefixes|) 
  (&optional reasoner) 

  (with-reasoner (reasoner)
    (let ((res nil))

      (maphash #'(lambda (key value)
                   (unless (is-default-prefix-p key)
                     (push 
                      (cons (format nil "~A" (without-colon key))
                            (format nil "~A" value))
                      res)))
	       (namespace-table *cur-reasoner*))

      (setf (last-answer *cur-reasoner*) 
            (cons (cons "defaultnamespace" (expand-prefix :defaultnamespace))
                  res))

      (return-by-policy))))

;;;
;;; 
;;; 

(defmethod print-object ((ax |OWLAxiom|) stream)
  (format stream "~S" (told ax)))

(defmethod unparse ((ax |OWLAxiom|))
  (told ax))

;;;
;;;
;;;

(defaxiom |OWLDatatypeDefinitionAxiom| (|OWLAxiom|)
 ((datatype-name :accessor datatype-name :initform nil :initarg :datatype-name)
  (data-range :accessor data-range :initform nil :initarg :data-range)))

(defmethod axioms-equal-p and ((ax1 |OWLDatatypeDefinitionAxiom|) (ax2 |OWLDatatypeDefinitionAxiom|))
  (and (default-slots-equal-p ax1 ax2 '(datatype-name))
       (datarange-slots-equal-p ax1 ax2 '(data-range))))

(defconstructor |OWLAPI-getOWLDatatypeDefinitionAxiom|
  (datatype-name data-range)
  
  (clear-entities datatype-name data-range)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDatatypeDefinitionAxiom| 
                    :told `(|OWLAPI-getOWLDatatypeDefinitionAxiom| 
                            ,datatype-name ,data-range ,reasoner)
                    :is-tbox-axiom-p t
                    :datatype-name (find-owl-entity datatype-name)
                    :data-range (find-owl-entity data-range)))))

(defmethod load-axiom :after ((axiom |OWLDatatypeDefinitionAxiom|) &key ontology) 
  (with-slots (datatype-name data-range) axiom
    (register-referenced-datatype datatype-name ontology)
    (register-referenced-datarange data-range ontology)))    

;;;
;;; 
;;;                      

(defaxiom |OWLHasKeyAxiom| (|OWLAxiom|)
  ((key-class :accessor key-class :initform nil :initarg :key-class)
   (key-object-properties :accessor key-object-properties :initform nil :initarg :key-object-properties)
   (key-data-properties :accessor key-data-properties :initform nil :initarg :key-data-properties)))

(defmethod axioms-equal-p and ((ax1 |OWLHasKeyAxiom|) (ax2 |OWLHasKeyAxiom|))
  (and (concept-slots-equal-p ax1 ax2 '(key-class))
       (list-slots-equal-p ax1 ax2 '(key-object-properties))
       (list-slots-equal-p ax1 ax2 '(key-data-properties))))                 

(defconstructor |OWLAPI-getOWLHasKeyAxiom|
  (key-class key-object-properties key-data-properties)

  (clear-entities key-class)
  (clear-entities1 key-object-properties)
  (clear-entities1 key-data-properties)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLHasKeyAxiom| 
                    :told `(|OWLAPI-getOWLHasKeyAxiom| ,key-class ,key-object-properties ,key-data-properties ,reasoner)
                    :is-tbox-axiom-p t
                    :key-class (find-owl-class key-class)
                    :key-object-properties (mapcar #'find-owl-property key-object-properties)
                    :key-data-properties (mapcar #'find-owl-property key-data-properties)))))
                    

;;;
;;; Annotation Axioms 
;;;

(defaxiom |OWLAxiomWithEntitySlotAxiom| (|OWLAxiom|)  ;; abstrakt
 ((entity :accessor entity :initform nil :initarg :entity)))   

(defmethod axioms-equal-p and ((ax1 |OWLAxiomWithEntitySlotAxiom|) (ax2 |OWLAxiomWithEntitySlotAxiom|))
  (or (typep ax1 '|OWLAxiomAnnotationAxiom|)
      ;; different axiom IDs dont count as difference 
      (default-slots-equal-p ax1 ax2 '(entity))))

(defaxiom |OWLAnnotationAxiom| (|OWLAxiomWithEntitySlotAxiom|)  ;; abstrakt
  ((annotation :accessor annotation :initform nil :initarg :annotation)))

(defmethod initialize-axiom :around ((ax |OWLAnnotationAxiom|) new-p)
  (declare (ignorable new-p))
  (unless (ignore-annotations-p *cur-reasoner*)
    (call-next-method)))

(defmethod load-axiom :around ((ax |OWLAnnotationAxiom|) &key ontology)
  (declare (ignorable ontology))
  (unless (ignore-annotations-p *cur-reasoner*)
    (call-next-method)))

(defmethod axioms-equal-p and ((ax1 |OWLAnnotationAxiom|) (ax2 |OWLAnnotationAxiom|))
  (default-slots-equal-p ax1 ax2 '(annotation)))

;;;
;;; 
;;; 

(defun process-annotations (abox entity annotations ontology)
  (unless (ignore-annotations-p *cur-reasoner*)

    (dolist (annotation annotations)

      (let ((annotation-role (second annotation))
            (annotation-value (third annotation))
            (tbox (owlapi-tbox *cur-reasoner*)))
      
        (reasoner-role-is-used-as-annotation-property annotation-role tbox)

        (register-referenced-annotation-property annotation-role ontology)        

        (reasoner-process-annotation entity annotation-role annotation-value abox)))))

;;;
;;; 
;;; 

(defaxiom |OWLAxiomAnnotationAxiom| (|OWLAnnotationAxiom|) ())

;; public OWLAxiomAnnotationAxiomImpl(OWLDataFactory dataFactory, OWLAxiom subject, OWLAnnotation annotation) {

(defmethod initialize-axiom :after ((ax |OWLAxiomAnnotationAxiom|) new-p)
  (when new-p
    (let ((annotated-ax (find-owl-axiom (second (told ax)))))
      (push ax (annotations annotated-ax)))))

(defconstructor |OWLAPI-getOWLAxiomAnnotationAxiom|
  (axiom-id annotation)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLAxiomAnnotationAxiom|
                    :told `(|OWLAPI-getOWLAxiomAnnotationAxiom| ,axiom-id ,annotation ,reasoner)
                    :entity (or (find-owl-axiom axiom-id)
                                (owlapi-runtime-error "Can't find axiom ~S" axiom-id))
                    :annotation (find-owl-entity annotation)))))

(defmethod load-axiom ((axiom |OWLAxiomAnnotationAxiom|) &key ontology)
  (declare (ignorable ontology))

  )

(defaxiom |OWLEntityAnnotationAxiom| (|OWLAnnotationAxiom|) ())

;; public OWLEntityAnnotationAxiomImpl(OWLDataFactory dataFactory, OWLEntity subject, OWLAnnotation annotation) {

(defconstructor |OWLAPI-getOWLEntityAnnotationAxiom|
  (entity annotation)
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLEntityAnnotationAxiom|
                    :told `(|OWLAPI-getOWLEntityAnnotationAxiom| ,entity ,annotation ,reasoner)
                    :entity (find-owl-entity entity)
                    :annotation (find-owl-entity annotation)))))

(defmethod load-axiom ((axiom |OWLEntityAnnotationAxiom|) &key ontology)
  (declare (ignorable ontology))

  (let* ((annotations (annotation axiom))
	 (annotations (if (consp (first annotations))
			  annotations
			(list annotations)))
	 (entity-types (ensure-list (first (entity axiom))))
	 (entity (second (entity axiom)))
	 (abox (owlapi-abox *cur-reasoner*)))

    (dolist (type entity-types)
      (case type
        ((|OWLClass| |Class|) (process-annotations abox entity annotations ontology))

        (|ObjectProperty| (process-annotations abox entity annotations ontology))
        (|DataProperty| (process-annotations abox entity annotations ontology))

        (|Annotation| (process-annotations abox entity annotations ontology))
        (|Datatype| (process-annotations abox entity annotations ontology))
        (|AnnotationProperty| (process-annotations abox entity annotations ontology))
        ((|Individual| |NamedIndividual|) (process-annotations abox entity annotations ontology))

        (otherwise (call-next-method))))))


(defaxiom |OWLOntologyAnnotationAxiom| (|OWLAnnotationAxiom|) ())

(defconstructor |OWLAPI-getOWLOntologyAnnotationAxiom|
  (annotation)
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLOntologyAnnotationAxiom|
                    :told `(|OWLAPI-getOWLOntologyAnnotationAxiom| 
                            ,annotation ,reasoner)
                    :annotation (find-owl-entity annotation)))))

(defmethod load-axiom ((axiom |OWLOntologyAnnotationAxiom|) &key ontology)  
  (declare (ignorable ontology))

  (let* ((annotations (annotation axiom))
	 (annotations (if (consp (first annotations))
			  annotations
			(list annotations)))
         (abox (owlapi-abox *cur-reasoner*)))

    (process-annotations abox
                         (name (entity axiom))
                         annotations
                         ontology)))

(defmethod load-axiom :before ((ax |OWLOntologyAnnotationAxiom|) &key ontology)
  (setf (entity ax) ontology))

(defmethod add-axiom :before ((ontology ontology) (ax |OWLOntologyAnnotationAxiom|))
  (setf (entity ax) ontology))

;;;
;;; new OWL Annotation Assertion Axioms 
;;; 

(defaxiom |OWLAnnotationAssertionAxiom| (|OWLAnnotationAxiom|) 
   ((annotation-subject :accessor annotation-subject :initform nil :initarg :annotation-subject)
    (annotation-value :accessor annotation-value :initform nil :initarg :annotation-value)
    (annotation-property :accessor annotation-property :initform nil :initarg :annotation-property)))


(defmethod axioms-equal-p and ((ax1 |OWLAnnotationAssertionAxiom|) (ax2 |OWLAnnotationAssertionAxiom|))
  (default-slots-equal-p ax1 ax2 '(annotation-subject annotation-value annotation-property)))


(defconstructor |OWLAPI-getOWLAnnotationAssertionAxiom|
  (annotation-subject annotation-property annotation-value)

  (clear-entities annotation-subject annotation-property annotation-value)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLAnnotationAssertionAxiom| 
                    :told `(|OWLAPI-getOWLAnnotationAssertionAxiom| 
                            ,annotation-subject ,annotation-property ,annotation-value ,reasoner)
                    :annotation-subject (find-owl-individual annotation-subject)
                    :annotation-property (find-owl-property annotation-property)
                    :annotation-value (find-owl-entity annotation-value)))))


(defmethod load-axiom ((axiom |OWLAnnotationAssertionAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-value annotation-subject annotation-property) axiom
    (let ((abox (owlapi-abox *cur-reasoner*)))
      (process-annotations abox 
                           annotation-subject 
                           (list (list annotation-subject 
                                       annotation-property 
                                       annotation-value))
                           ontology))))

;;;
;;;
;;; 

(defaxiom |OWLSubAnnotationPropertyOfAxiom| (|OWLAnnotationAxiom|)
   ((annotation-sub-property :accessor annotation-sub-property :initform nil :initarg :annotation-sub-property)
    (annotation-super-property :accessor annotation-super-property :initform nil :initarg :annotation-super-property)))

(defmethod axioms-equal-p and ((ax1 |OWLSubAnnotationPropertyOfAxiom|) (ax2 |OWLSubAnnotationPropertyOfAxiom|))
  (default-slots-equal-p ax1 ax2 '(annotation-sub-property annotation-super-property)))


(defconstructor |OWLAPI-getOWLSubAnnotationPropertyOfAxiom|
  (annotation-sub-property annotation-super-property)

  (clear-entities annotation-sub-property annotation-super-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLSubAnnotationPropertyOfAxiom| 
                    :told `(|OWLAPI-getOWLSubAnnotationPropertyOfAxiom|
                            ,annotation-sub-property ,annotation-super-property ,reasoner)
                    :is-tbox-axiom-p t
                    :annotation-sub-property (find-owl-property annotation-sub-property)
                    :annotation-super-property (find-owl-property annotation-super-property)))))


(owlapi-defun (|OWLAPI-getOWLSubAnnotationPropertyAxiom|) (&rest args)
  (apply #'|OWLAPI-getOWLSubAnnotationPropertyOfAxiom| args))

(defmethod load-axiom :after ((axiom |OWLSubAnnotationPropertyOfAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-sub-property annotation-super-property) axiom
    (register-referenced-annotation-property annotation-sub-property ontology)
    (register-referenced-annotation-property annotation-super-property ontology)))

;;;
;;;
;;;

(defaxiom |OWLAnnotationPropertyDomainAxiom| (|OWLAnnotationAxiom|)
   ((annotation-property1 :accessor annotation-property1 :initform nil :initarg :annotation-property1)
    (annotation-property-domain :accessor annotation-property-domain :initform nil :initarg :annotation-property-domain)))

(defmethod axioms-equal-p and ((ax1 |OWLAnnotationPropertyDomainAxiom|) (ax2 |OWLAnnotationPropertyDomainAxiom|))
  (and (default-slots-equal-p ax1 ax2 '(annotation-property1))
       (concept-slots-equal-p ax1 ax2 '(annotation-property-domain))))


(defconstructor |OWLAPI-getOWLAnnotationPropertyDomainAxiom|
  (annotation-property annotation-property-domain)

  (clear-entities annotation-property annotation-property-domain)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLAnnotationPropertyDomainAxiom| 
                    :told `(|OWLAPI-getOWLAnnotationPropertyDomainAxiom|
                            ,annotation-property ,annotation-property-domain ,reasoner)
                    :is-tbox-axiom-p t
                    :annotation-property1 (find-owl-property annotation-property)
                    :annotation-property-domain (find-owl-class annotation-property-domain)))))

(defmethod load-axiom :after ((axiom |OWLAnnotationPropertyDomainAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-property1 annotation-property-domain) axiom
    (register-referenced-annotation-property annotation-property1 ontology)
    (register-referenced-concept annotation-property-domain ontology)))

;;;
;;;
;;;

(defaxiom |OWLAnnotationPropertyRangeAxiom| (|OWLAnnotationAxiom|)
   ((annotation-property2 :accessor annotation-property2 :initform nil :initarg :annotation-property2)
    (annotation-property-range :accessor annotation-property-range :initform nil :initarg :annotation-property-range)))

(defmethod axioms-equal-p and ((ax1 |OWLAnnotationPropertyRangeAxiom|) (ax2 |OWLAnnotationPropertyRangeAxiom|))
  (and (default-slots-equal-p ax1 ax2 '(annotation-property2))
       (concept-slots-equal-p ax1 ax2 '(annotation-property-range))))
          
(defconstructor |OWLAPI-getOWLAnnotationPropertyRangeAxiom|
  (annotation-property annotation-property-range)

  (clear-entities annotation-property annotation-property-range)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLAnnotationPropertyRangeAxiom| 
                    :told `(|OWLAPI-getOWLAnnotationPropertyRangeAxiom|
                            ,annotation-property ,annotation-property-range ,reasoner)
                    :is-tbox-axiom-p t
                    :annotation-property2 (find-owl-property annotation-property)
                    :annotation-property-range (find-owl-class annotation-property-range)))))

(defmethod load-axiom :after ((axiom |OWLAnnotationPropertyRangeAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-property2 annotation-property-range) axiom
    (register-referenced-datarange annotation-property-range ontology)
    (register-referenced-annotation-property annotation-property2 ontology)))

;;;
;;;
;;; 

(defun find-entity (args)
  (find-if #'is-entity-p args))

(defun is-entity-p (arg)
  (and (consp arg)
       (member (first arg)
               '(|Datatype| 
                 |OWLClass|
                 |Class|
                 |ObjectProperty|
                 |AnnnotationProperty|
                 |DataProperty|
                 |Individual|
                 |NamedIndividual|))))


;;;
;;; Declaration Axioms
;;; 

(defaxiom |OWLDeclarationAxiom| (|OWLAxiomWithEntitySlotAxiom|) ())

(defaxiom |OWLImplicitDeclarationAxiom| (|OWLDeclarationAxiom|) ())

(defaxiom |OWLReallyImplicitDeclarationAxiom| (|OWLImplicitDeclarationAxiom|) ())

(defmethod load-axiom :after ((axiom |OWLDeclarationAxiom|) &key ontology)
  (declare (ignorable ontology))

  (flet ((find-key (key spec)
           (if (consp spec)
               (find key spec)
             (eq key spec))))

    (with-slots (entity) axiom
      (cond ((find-key '|OWLClass| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-concept (second entity) ontology)))
            ((find-key '|Class| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-concept (second entity) ontology)))
            ((find-key '|ObjectProperty| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-object-property (second entity) ontology)))
            ((find-key '|DataProperty| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-data-property (second entity) ontology)))
            ((find-key '|AnnotationProperty| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-annotation-property (second entity) ontology)))
            ((find-key '|Individual| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-individual (second entity) ontology)))
            ((find-key '|NamedIndividual| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-individual (second entity) ontology)))
            ((find-key '|Datatype| (first entity))
             (when (and (not (ignore-declarations-p *cur-reasoner*))
                        (not (typep axiom '|OWLReallyImplicitDeclarationAxiom|)))
               (register-declared-datatype (second entity) ontology))
             )
            (t (error "Key ~A unknown." (first entity)))))))

(defmethod axioms-equal-p and ((ax1 |OWLDeclarationAxiom|) (ax2 |OWLDeclarationAxiom|))
  (default-slots-equal-p ax1 ax2 '(entity)))

(defmethod entailed-p ((axiom |OWLDeclarationAxiom|))
  nil)

;; public OWLDeclarationAxiomImpl(OWLDataFactory dataFactory, OWLEntity entity) {

(defconstructor |OWLAPI-getOWLDeclarationAxiom| (entity)
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDeclarationAxiom|
                    :told `(|OWLAPI-getOWLDeclarationAxiom| ,entity ,reasoner)
                    :entity (find-owl-entity entity)))))

(defconstructor |OWLAPI-getOWLImplicitDeclarationAxiom| (entity)
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLImplicitDeclarationAxiom|
                    :told `(|OWLAPI-getOWLImplicitDeclarationAxiom| ,entity ,reasoner)
                    :entity (find-owl-entity entity)))))
   
(defconstructor |OWLAPI-getOWLReallyImplicitDeclarationAxiom| (entity)
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLReallyImplicitDeclarationAxiom|
                    :told `(|OWLAPI-getOWLReallyImplicitDeclarationAxiom| ,entity ,reasoner)
                    :entity (find-owl-entity entity)))))
   
;;;
;;; 
;;; 

(defaxiom |OWLOntologyURIAttributeAxiom| (|OWLAxiom|) ; abstrakt 
  ((uri :accessor uri :initform nil :initarg :uri)))

(defmethod axioms-equal-p and ((ax1 |OWLOntologyURIAttributeAxiom|) (ax2 |OWLOntologyURIAttributeAxiom|))
  (default-slots-equal-p ax1 ax2 '(uri)))


;;;
;;;
;;; 

(defaxiom |OWLImportsDeclarationAxiom| (|OWLOntologyURIAttributeAxiom|) ())

;; public OWLImportsDeclarationImpl(OWLDataFactory dataFactory, OWLOntology subject, URI importsURI) {

(defconstructor |OWLAPI-getOWLImportsDeclarationAxiom| (ontology-import-uri)

  (clear-entities ontology-import-uri)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLImportsDeclarationAxiom|
                    :told `(|OWLAPI-getOWLImportsDeclarationAxiom| ,ontology-import-uri ,reasoner)
                    :uri (find-owl-entity ontology-import-uri)))))

(defmethod load-axiom ((axiom |OWLImportsDeclarationAxiom|) &key ontology)
  (declare (ignorable ontology))

  t)

;;;
;;;
;;; 

(defaxiom |OWLOntologyVersionDeclarationAxiom| (|OWLOntologyURIAttributeAxiom|) ())

(defmethod load-axiom ((axiom |OWLOntologyVersionDeclarationAxiom|) &key ontology)
  (declare (ignorable ontology))

  t)

(defconstructor |OWLAPI-getOWLOntologyVersionDeclarationAxiom| (ontology-version-uri)

  (clear-entities ontology-version-uri)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLOntologyVersionDeclarationAxiom|
                    :told `(|OWLAPI-getOWLOntologyVersionDeclarationAxiom| ,ontology-version-uri
                                                                           ,reasoner)
                    :uri (find-owl-entity ontology-version-uri)))))

;;;
;;; Namespace Axiom
;;; 

(defaxiom |OWLPrefixDeclarationAxiom| (|OWLOntologyURIAttributeAxiom|)
  ((prefix :accessor prefix :initform nil :initarg :prefix)))

(defmethod axioms-equal-p and ((ax1 |OWLPrefixDeclarationAxiom|) (ax2 |OWLPrefixDeclarationAxiom|))
  (default-slots-equal-p ax1 ax2 '(prefix)))

(defmethod load-axiom ((axiom |OWLPrefixDeclarationAxiom|) &key ontology)
  (declare (ignorable ontology))

  t)

(defconstructor |OWLAPI-getOWLPrefixDeclarationAxiom| (namespace-prefix namespace)
  (with-reasoner (reasoner)
    
    (|OWLAPI-addPrefix| namespace-prefix namespace reasoner)

    (return-policy-new-axiom
     (make-instance '|OWLPrefixDeclarationAxiom|
                    :told `(|OWLAPI-getOWLPrefixDeclarationAxiom| 
                            ,namespace-prefix ,namespace ,reasoner)
                    :uri (find-owl-entity namespace)
                    :prefix (find-owl-entity namespace-prefix)))))

;;;
;;; Logical Axioms
;;; 

(defaxiom |OWLLogicalAxiom| (|OWLAxiom|) ())  ;; abstract

(defaxiom |OWLClassAxiom| (|OWLLogicalAxiom|) ());; abstract

(defaxiom |OWLNaryClassAxiom| (|OWLClassAxiom|)  ;; abstract 
  ((descriptions :accessor descriptions :initform nil :initarg :descriptions)))

(defmethod axioms-equal-p and ((ax1 |OWLNaryClassAxiom|) (ax2 |OWLNaryClassAxiom|))
  (set-of-concepts-slots-equal-p ax1 ax2 '(descriptions)))

;;;
;;;
;;;

(defun pairwise-equivalent-p (list predicate)
  (every #'(lambda (x y)
             (funcall predicate x y))
         list (rest list)))


(defun for-all-pairs-holds-p (list predicate)
  (let ((res t))
    (block block
      (mapl #'(lambda (first)
                (let ((a (first first)))
                  (unless res
                    (return-from block nil))
                  (setf res
                        (and res
                             (every #'(lambda (b)
                                        (and res 
                                             (funcall predicate a b)))
                                    (rest first))))))
            list))
    res))

;;;
;;; Class Axioms: Disjoint, Disjoint Union, Equivalent, Subclass 
;;; 
  
(defaxiom |OWLDisjointClassesAxiom| (|OWLNaryClassAxiom|) ())

;; public OWLDisjointClassesAxiomImpl(OWLDataFactory dataFactory, Set<? extends OWLDescription> descriptions) {

(defconstructor |OWLAPI-getOWLDisjointClassesAxiom| (descriptions)

  (clear-entities1 descriptions)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDisjointClassesAxiom| 
                    :told `(|OWLAPI-getOWLDisjointClassesAxiom| ,descriptions ,reasoner)
                    :is-tbox-axiom-p t
                    :descriptions (mapcar #'(lambda (x) 
                                              (find-owl-class x))
                                          descriptions)))))

(defmethod load-axiom :after ((axiom |OWLDisjointClassesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (mapc #'declare-datatype-properties descriptions)
    (mapc #'(lambda (x) (register-referenced-concept x ontology)) descriptions)))

;;;
;;;
;;; 

(defaxiom |OWLDisjointUnionAxiom| (|OWLNaryClassAxiom|) ())

;; public OWLDisjointUnionAxiomImpl(OWLDataFactory dataFactory, OWLClass owlClass,
;;                                  Set<? extends OWLDescription> descriptions) {

(defconstructor |OWLAPI-getOWLDisjointUnionAxiom| (description descriptions)

  (clear-entities description)
  (clear-entities1 descriptions)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDisjointUnionAxiom|
                    :told `(|OWLAPI-getOWLDisjointUnionAxiom| ,description ,descriptions ,reasoner)
                    ;;; owlClass = U+ descriptions 
                    :is-tbox-axiom-p t
                    :descriptions (mapcar #'(lambda (x) 
                                              (find-owl-class x))
                                          (cons description descriptions))))))


(defmethod load-axiom :after ((axiom |OWLDisjointUnionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (mapc #'declare-datatype-properties descriptions)    
    (mapc #'(lambda (x) (register-referenced-concept x ontology)) descriptions)))


;;;
;;;
;;; 
  
(defaxiom |OWLEquivalentClassesAxiom| (|OWLNaryClassAxiom|) ())

;; public OWLEquivalentClassesAxiomImpl(OWLDataFactory dataFactory, Set<? extends OWLDescription> descriptions) {


(defconstructor |OWLAPI-getOWLEquivalentClassesAxiom| (descriptions)

  (clear-entities1 descriptions)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLEquivalentClassesAxiom|
                    :told `(|OWLAPI-getOWLEquivalentClassesAxiom| ,descriptions ,reasoner)
                    :is-tbox-axiom-p t
                    :descriptions (mapcar #'(lambda (x) 
                                              (find-owl-class x))
                                          descriptions)))))


(defmethod load-axiom :after ((axiom |OWLEquivalentClassesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (mapc #'declare-datatype-properties descriptions)
    (mapc #'(lambda (x) (register-referenced-concept x ontology)) descriptions)))


;;;
;;;
;;; 

(defaxiom |OWLSubClassAxiom| (|OWLNaryClassAxiom|) ())

;; public OWLSubClassAxiomImpl(OWLDataFactory dataFactory, OWLDescription subClass,
;;                             OWLDescription superClass) {


(defconstructor |OWLAPI-getOWLSubClassAxiom| (sub-class super-class)

  (clear-entities sub-class super-class)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLSubClassAxiom|
                    :told `(|OWLAPI-getOWLSubClassAxiom| ,sub-class ,super-class ,reasoner)
                    :is-tbox-axiom-p t
                    :descriptions (mapcar #'(lambda (x) (find-owl-class x))
                                          (list sub-class super-class))))))


#|
(owlapi-defun (|SubClassOf|) (sub-class super-class &optional reasoner)
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLSubClassAxiom|
                    :told `(|OWLAPI-getOWLSubClassAxiom| ,sub-class ,super-class ,reasoner)
                    :is-tbox-axiom-p t
                    :descriptions (mapcar #'(lambda (x) (find-owl-class x))
                                          (list sub-class super-class))))))
|#

(defmethod load-axiom :after ((axiom |OWLSubClassAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (mapc #'declare-datatype-properties descriptions)
    (mapc #'(lambda (x) (register-referenced-concept x ontology)) descriptions)))


;;;
;;; Individual Axioms 
;;;

(defaxiom |OWLIndividualAxiom| (|OWLLogicalAxiom|) ()) ;; abstrakt

(defaxiom |OWLNaryIndividualAxiom| (|OWLIndividualAxiom|) ;; abstract 
  ((individuals :accessor individuals :initform nil :initarg :individuals)))

(defmethod axioms-equal-p and ((ax1 |OWLNaryIndividualAxiom|) (ax2 |OWLNaryIndividualAxiom|))
  (set-slots-equal-p ax1 ax2 '(individuals)))

;;;
;;; Class Assertion
;;; 

(defaxiom |OWLClassAssertionAxiom| (|OWLIndividualAxiom|) 
  ((description :accessor description :initform nil :initarg :description)
   (ax-individual :accessor ax-individual :initform nil :initarg :ax-individual)))

(defmethod axioms-equal-p and ((ax1 |OWLClassAssertionAxiom|) (ax2 |OWLClassAssertionAxiom|))
  (and (default-slots-equal-p ax1 ax2 '(ax-individual))
       (concept-slots-equal-p ax1 ax2 '(description))))

;; public OWLClassAssertionAxiomImpl(OWLDataFactory dataFactory, OWLIndividual individual, OWLDescription description) {

(defconstructor |OWLAPI-getOWLClassAssertionAxiom| (individual description)

  (clear-entities individual description)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLClassAssertionAxiom|
                    :told `(|OWLAPI-getOWLClassAssertionAxiom| ,individual ,description ,reasoner)
                    :ax-individual (find-owl-individual individual)
                    :description (find-owl-class description)))))
                  
(defmethod load-axiom :after ((axiom |OWLClassAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (ax-individual description) axiom
    (declare-datatype-properties description)

    (register-referenced-individual ax-individual ontology)
    (register-referenced-concept description ontology)))



;;;
;;; Individual Axioms: Different From, Same As
;;; 


(defaxiom |OWLDifferentIndividualsAxiom| (|OWLNaryIndividualAxiom|) ())

;; public OWLDifferentIndividualsAxiomImpl(OWLDataFactory dataFactory, Set<OWLIndividual> individuals) {

(defconstructor |OWLAPI-getOWLDifferentIndividualsAxiom| (individuals)
  
  (clear-entities1 individuals)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDifferentIndividualsAxiom|
                    :told `(|OWLAPI-getOWLDifferentIndividualsAxiom| ,individuals ,reasoner)
                    :individuals (mapcar #'(lambda (x) (find-owl-individual x)) individuals)))))

(defmethod load-axiom :after ((axiom |OWLDifferentIndividualsAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (individuals) axiom
    (mapc #'(lambda (x) (register-referenced-individual x ontology)) individuals)))



;;;
;;;
;;; 

(defaxiom |OWLSameIndividualsAxiom| (|OWLNaryIndividualAxiom|) ())

;;  public OWLSameIndividualsAxiomImpl(OWLDataFactory dataFactory, Set<OWLIndividual> individuals) {

(defconstructor |OWLAPI-getOWLSameIndividualsAxiom| (individuals)
  
  (clear-entities1 individuals)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLSameIndividualsAxiom|
                    :told `(|OWLAPI-getOWLSameIndividualsAxiom| ,individuals ,reasoner)
                    :individuals (mapcar #'(lambda (x) (find-owl-individual x)) individuals)))))


(defmethod load-axiom :after ((axiom |OWLSameIndividualsAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (individuals) axiom
    (mapc #'(lambda (x) (register-referenced-individual x ontology)) individuals)))


;;;
;;; Individual Relationships 
;;;

(defaxiom |OWLIndividualRelationshipAxiom| (|OWLIndividualAxiom|) ;; abstrakt 
  ((subject :accessor subject :initform nil :initarg :subject)))

(defmethod axioms-equal-p and ((ax1 |OWLIndividualRelationshipAxiom|) (ax2 |OWLIndividualRelationshipAxiom|))
  (default-slots-equal-p ax1 ax2 '(subject)))

;;;
;;; Data Property Assertion Axioms 
;;; 

(defaxiom |OWLDataPropertyAssertionAxiom| (|OWLIndividualRelationshipAxiom|) 
  ((rel-data-property :accessor rel-data-property :initform nil :initarg :rel-data-property)
   (data-literal :accessor data-literal :initform nil :initarg :data-literal)))

(defmethod axioms-equal-p and ((ax1 |OWLDataPropertyAssertionAxiom|) (ax2 |OWLDataPropertyAssertionAxiom|))
  (default-slots-equal-p ax1 ax2 '(rel-data-property data-literal)))


;;  public OWLDataPropertyAssertionAxiomImpl(OWLDataFactory dataFactory, OWLIndividual subject, OWLDataPropertyExpression property,
;;                                             OWLConstant value) {

(defconstructor |OWLAPI-getOWLDataPropertyAssertionAxiom| (subject rel-data-property value)

  (clear-entities subject rel-data-property value)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDataPropertyAssertionAxiom|
                    :told `(|OWLAPI-getOWLDataPropertyAssertionAxiom| ,subject ,rel-data-property ,value ,reasoner)
                    :subject (find-owl-individual subject)
                    :rel-data-property (find-owl-property rel-data-property)
                    :data-literal (find-owl-entity value)))))


(defmethod load-axiom :after  ((axiom |OWLDataPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-data-property) axiom
    (register-referenced-individual subject ontology)
    (register-referenced-data-property rel-data-property ontology)))


;;;
;;;
;;;   

(defaxiom |OWLNegativeDataPropertyAssertionAxiom| (|OWLDataPropertyAssertionAxiom|) ())

;; public OWLNegativeDataPropertyAssertionAxiomImpl(OWLDataFactory dataFactory, OWLIndividual subject, OWLDataPropertyExpression property,
;;                                                     OWLConstant object) {

(defconstructor |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom| (subject rel-data-property value)

  (clear-entities subject rel-data-property value)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLNegativeDataPropertyAssertionAxiom|
                    :told `(|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom| ,subject ,rel-data-property ,value ,reasoner)
                    :subject (find-owl-individual subject)
                    :rel-data-property (find-owl-property rel-data-property)
                    :data-literal (find-owl-entity value)))))

(defmethod load-axiom :after  ((axiom |OWLNegativeDataPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-data-property) axiom
    (register-referenced-individual subject ontology)
    (register-referenced-data-property rel-data-property ontology)))


;;;
;;; Object Property Assertion 
;;; 

(defaxiom |OWLObjectPropertyAssertionAxiom| (|OWLIndividualRelationshipAxiom|) 
  ((rel-object-property :accessor rel-object-property :initform nil :initarg :rel-object-property)
   (object :accessor object :initform nil :initarg :object)))

(defmethod axioms-equal-p and ((ax1 |OWLObjectPropertyAssertionAxiom|) (ax2 |OWLObjectPropertyAssertionAxiom|))
  (default-slots-equal-p ax1 ax2 '(rel-object-property object)))


;; public OWLObjectPropertyAssertionAxiomImpl(OWLDataFactory dataFactory, OWLIndividual subject, OWLObjectPropertyExpression property,
;;                                               OWLIndividual object) {

(defconstructor |OWLAPI-getOWLObjectPropertyAssertionAxiom| (subject rel-object-property object)

  (clear-entities subject rel-object-property object)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLObjectPropertyAssertionAxiom|
                    :told `(|OWLAPI-getOWLObjectPropertyAssertionAxiom| ,subject ,rel-object-property ,object ,reasoner)
                    :subject (find-owl-individual subject)
                    :rel-object-property (find-owl-property rel-object-property)
                    :object (find-owl-individual object)))))

(defmethod load-axiom :after ((axiom |OWLObjectPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-object-property object) axiom
    (register-referenced-individual subject ontology)
    (register-referenced-object-property rel-object-property ontology)
    (register-referenced-individual object ontology)))


;;;
;;;
;;;

(defaxiom |OWLNegativeObjectPropertyAssertionAxiom| (|OWLObjectPropertyAssertionAxiom|) ())

;; public OWLNegativeObjectPropertyAssertionAxiomImpl(OWLDataFactory dataFactory, OWLIndividual subject, 
;;                                                       OWLObjectPropertyExpression property,
;;                                                       OWLIndividual object) {


(defconstructor |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| (subject rel-object-property object)

  (clear-entities subject rel-object-property object)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLNegativeObjectPropertyAssertionAxiom|
                    :told `(|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| 
                            ,subject ,rel-object-property ,object ,reasoner)
                    :subject (find-owl-individual subject)
                    :rel-object-property (find-owl-property rel-object-property)
                    :object (find-owl-individual object)))))


(defmethod load-axiom :after ((axiom |OWLNegativeObjectPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))
  (with-slots (subject rel-object-property object) axiom
    (register-referenced-individual subject ontology)
    (register-referenced-individual object ontology)
    (register-referenced-object-property rel-object-property ontology)))


;;;
;;; Property Axioms / Characteristics: Functional, Inverse Functional, Transitive, Reflexive, Irreflexive, Subproperties, Complex Role Inclusion Axioms
;;; 

(defaxiom |OWLPropertyAxiom| (|OWLLogicalAxiom|) ())  ;; abstrakt

(defaxiom |OWLObjectPropertyAxiom| (|OWLPropertyAxiom|)  ;; abstrakt
 ((object-property :accessor object-property :initform nil :initarg :object-property)))

(defmethod axioms-equal-p and ((ax1 |OWLObjectPropertyAxiom|) (ax2 |OWLObjectPropertyAxiom|))
  (default-slots-equal-p ax1 ax2 '(object-property)))

(defaxiom |OWLDataPropertyAxiom| (|OWLPropertyAxiom|)  ;; abstrakt
 ((data-property :accessor data-property :initform nil :initarg :data-property)))

(defmethod axioms-equal-p and ((ax1 |OWLDataPropertyAxiom|) (ax2 |OWLDataPropertyAxiom|))
  (default-slots-equal-p ax1 ax2 '(data-property)))

(defaxiom |OWLNaryObjectPropertyAxiom| (|OWLPropertyAxiom|) ;; abstrakt
   ((object-properties :accessor object-properties :initform nil :initarg :object-properties)))

(defmethod axioms-equal-p and ((ax1 |OWLNaryObjectPropertyAxiom|) (ax2 |OWLNaryObjectPropertyAxiom|))
  (set-slots-equal-p ax1 ax2 '(object-properties)))

(defaxiom |OWLNaryDataPropertyAxiom| (|OWLPropertyAxiom|) ;; abstrakt
   ((data-properties :accessor data-properties :initform nil :initarg :data-properties)))

(defmethod axioms-equal-p and ((ax1 |OWLNaryDataPropertyAxiom|) (ax2 |OWLNaryDataPropertyAxiom|))
  (set-slots-equal-p ax1 ax2 '(data-properties)))

;;;
;;; Datatype + Object Properties: Characteristics 
;;; 

(defaxiom |OWLFunctionalDataPropertyAxiom| (|OWLDataPropertyAxiom|) ()) 

;;  public OWLFunctionalDataPropertyAxiomImpl(OWLDataFactory dataFactory, OWLDataPropertyExpression property) {

(defconstructor |OWLAPI-getOWLFunctionalDataPropertyAxiom| (data-property)

  (clear-entities data-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLFunctionalDataPropertyAxiom|
                    :told `(|OWLAPI-getOWLFunctionalDataPropertyAxiom| ,data-property ,reasoner)
                    :is-tbox-axiom-p t
                    :data-property (find-owl-property data-property)))))

(defmethod load-axiom :after ((axiom |OWLFunctionalDataPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-property) axiom
    (register-referenced-data-property data-property ontology)))


;;;
;;;
;;;

(defaxiom |OWLFunctionalObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ())

;; public OWLFunctionalObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLFunctionalObjectPropertyAxiom| (object-property)

  (clear-entities object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLFunctionalObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLFunctionalObjectPropertyAxiom| ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))

(defmethod load-axiom :after ((axiom |OWLFunctionalObjectPropertyAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))

;;;
;;;
;;;

(defaxiom |OWLDisjointDataPropertiesAxiom| (|OWLNaryDataPropertyAxiom|) ())

;; public OWLDisjointDataPropertiesAxiomImpl(OWLDataFactory dataFactory, Set<? extends OWLDataPropertyExpression> properties) {

(defconstructor |OWLAPI-getOWLDisjointDataPropertiesAxiom| (data-properties)

  (clear-entities1 data-properties)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDisjointDataPropertiesAxiom|
                    :told `(|OWLAPI-getOWLDisjointDataPropertiesAxiom| ,data-properties ,reasoner)
                    :is-tbox-axiom-p t
                    :data-properties (mapcar #'(lambda (x) (find-owl-property x))
                                        data-properties)))))


(defmethod load-axiom :after ((axiom |OWLDisjointDataPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-properties) axiom

  (mapc #'(lambda (role)
            (register-referenced-data-property role ontology))
        data-properties)))

;;;
;;;
;;; 

(defaxiom |OWLDisjointObjectPropertiesAxiom| (|OWLNaryObjectPropertyAxiom|) ())

;; public OWLDisjointObjectPropertiesAxiomImpl(OWLDataFactory dataFactory, Set<? extends OWLObjectPropertyExpression> properties) {

(defconstructor |OWLAPI-getOWLDisjointObjectPropertiesAxiom| (object-properties)

  (clear-entities1 object-properties)
  
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLDisjointObjectPropertiesAxiom|
                    :told `(|OWLAPI-getOWLDisjointObjectPropertiesAxiom| ,object-properties ,reasoner)
                    :is-tbox-axiom-p t
                    :object-properties (mapcar #'(lambda (x) (find-owl-property x)) 
                                               object-properties)))))


(defmethod load-axiom :after ((axiom |OWLDisjointObjectPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-properties) axiom

  (mapc #'(lambda (role)
              (register-referenced-object-property role ontology))
        object-properties)))


;;;
;;;
;;; 

(defaxiom |OWLEquivalentDataPropertiesAxiom| (|OWLNaryDataPropertyAxiom|) ()) 

;; public OWLEquivalentDataPropertiesAxiomImpl(OWLDataFactory dataFactory, Set<? extends OWLDataPropertyExpression> properties) {

(defconstructor |OWLAPI-getOWLEquivalentDataPropertiesAxiom| (data-properties)

  (clear-entities1 data-properties)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLEquivalentDataPropertiesAxiom|
                    :told `(|OWLAPI-getOWLEquivalentDataPropertiesAxiom| ,data-properties ,reasoner)
                    :is-tbox-axiom-p t
                    :data-properties (mapcar #'(lambda (x) (find-owl-property x)) data-properties)))))

(defmethod load-axiom :after ((axiom |OWLEquivalentDataPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-properties) axiom
    (mapc #'(lambda (r) 
              (register-referenced-data-property r ontology))
          data-properties)))

;;;
;;;
;;;

(defaxiom |OWLEquivalentObjectPropertiesAxiom| (|OWLNaryObjectPropertyAxiom|) ())

;; public OWLEquivalentObjectPropertiesAxiomImpl(OWLDataFactory dataFactory, Set<? extends OWLObjectPropertyExpression> properties) {

(defconstructor |OWLAPI-getOWLEquivalentObjectPropertiesAxiom| (object-properties)

  (clear-entities1 object-properties)
  
  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLEquivalentObjectPropertiesAxiom|
                    :told `(|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| ,object-properties ,reasoner)
                    :is-tbox-axiom-p t
                    :object-properties (mapcar #'(lambda (x) (find-owl-property x)) object-properties)))))


(defmethod load-axiom :after ((axiom |OWLEquivalentObjectPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-properties) axiom
    (mapc #'(lambda (x) 
              (register-referenced-object-property x ontology))
          object-properties)))


;;;
;;;
;;; 

(defaxiom |OWLInverseObjectPropertiesAxiom| (|OWLNaryObjectPropertyAxiom|) ())

;; public OWLInverseObjectPropertiesAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression first,
;;                                               OWLObjectPropertyExpression second) {

(defconstructor |OWLAPI-getOWLInverseObjectPropertiesAxiom| (first-object-property second-object-property)
  
  (clear-entities first-object-property second-object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLInverseObjectPropertiesAxiom|
                    :told `(|OWLAPI-getOWLInverseObjectPropertiesAxiom| 
                            ,first-object-property ,second-object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-properties (list (find-owl-property first-object-property)
                                      (find-owl-property second-object-property))))))

(defmethod load-axiom :after ((axiom |OWLInverseObjectPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-properties) axiom
    (let* ((r1 (first object-properties))
           (r2 (second object-properties)))

      (register-referenced-object-property r1 ontology)
      (register-referenced-object-property r2 ontology))))

;;;
;;;
;;; 

(defaxiom |OWLObjectPropertyChainSubPropertyAxiom| (|OWLObjectPropertyAxiom|)
  ((object-property-chain :accessor object-property-chain :initform nil :initarg :object-property-chain)))

(defmethod axioms-equal-p and ((ax1 |OWLObjectPropertyChainSubPropertyAxiom|) (ax2 |OWLObjectPropertyChainSubPropertyAxiom|))
  (list-slots-equal-p ax1 ax2 '(object-property-chain)))
   
;; public OWLObjectPropertyChainSubPropertyAxiomImpl(OWLDataFactory dataFactory,
;;                                                      List<? extends OWLObjectPropertyExpression> propertyChain,
;;                                                      OWLObjectPropertyExpression superProperty) {

(defconstructor |OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| (object-property-chain object-super-property) 

  (clear-entities object-super-property)
  (clear-entities1 object-property-chain)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLObjectPropertyChainSubPropertyAxiom|
                    :told `(|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| 
                            ,object-property-chain ,object-super-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-super-property)
                    :object-property-chain (mapcar #'(lambda (x) (find-owl-property x)) object-property-chain)))))

(defmethod load-axiom :after ((axiom |OWLObjectPropertyChainSubPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property-chain object-property) axiom
    (register-referenced-object-property object-property ontology)
    (mapc #'(lambda (x) 
              (register-referenced-object-property x ontology))
          object-property-chain)))

;;;
;;;
;;; 

(defaxiom |OWLAsymmetricObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ())

;; public OWLAntiSymmetricObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLAsymmetricObjectPropertyAxiom| (object-property)

  (clear-entities object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom
     (make-instance '|OWLAsymmetricObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))

(defmethod load-axiom :after ((axiom |OWLAsymmetricObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))


;;;
;;;
;;; 

(defaxiom |OWLInverseFunctionalObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ())

;; public OWLInverseFunctionalObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| (object-property)

  (clear-entities object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLInverseFunctionalObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|
                            ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))

(defmethod load-axiom :after ((axiom |OWLInverseFunctionalObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))


;;;
;;;
;;;

(defaxiom |OWLIrreflexiveObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ())

;; public OWLIrreflexiveObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| (object-property)

  (clear-entities object-property) 

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLIrreflexiveObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))

(defmethod load-axiom :after ((axiom |OWLIrreflexiveObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))


;;;
;;;
;;;

(defaxiom |OWLReflexiveObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ())

;; public OWLReflexiveObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLReflexiveObjectPropertyAxiom| (object-property)

  (clear-entities object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLReflexiveObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLReflexiveObjectPropertyAxiom| ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))

(defmethod load-axiom :after ((axiom |OWLReflexiveObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))


;;;
;;;
;;;

(defaxiom |OWLSymmetricObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ()) 

;; public OWLSymmetricObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLSymmetricObjectPropertyAxiom| (object-property)

  (clear-entities object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLSymmetricObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLSymmetricObjectPropertyAxiom| ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))


(defmethod load-axiom :after ((axiom |OWLSymmetricObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))


;;;
;;;
;;;

(defaxiom |OWLTransitiveObjectPropertyAxiom| (|OWLObjectPropertyAxiom|) ())

;; public OWLTransitiveObjectPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property) {

(defconstructor |OWLAPI-getOWLTransitiveObjectPropertyAxiom| (object-property)

  (clear-entities object-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLTransitiveObjectPropertyAxiom|
                    :told `(|OWLAPI-getOWLTransitiveObjectPropertyAxiom| ,object-property ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)))))

(defmethod load-axiom :after ((axiom |OWLTransitiveObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (register-referenced-object-property object-property ontology)))


;;;
;;; Data + Object Properties: Sub Properties, Domain, Range 
;;; 

(defaxiom |OWLObjectSubPropertyAxiom| (|OWLObjectPropertyAxiom|) 
   ((sub-object-property :accessor sub-object-property :initform nil :initarg :sub-object-property))) 

(defmethod axioms-equal-p and ((ax1 |OWLObjectSubPropertyAxiom|) (ax2 |OWLObjectSubPropertyAxiom|))
  (default-slots-equal-p ax1 ax2 '(sub-object-property)))

;; public OWLObjectSubPropertyAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression subProperty,
;;                                         OWLObjectPropertyExpression superProperty) {

(defconstructor |OWLAPI-getOWLObjectSubPropertyAxiom| (object-sub-property object-super-property)

  (clear-entities object-sub-property object-super-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLObjectSubPropertyAxiom|
                    :told `(|OWLAPI-getOWLObjectSubPropertyAxiom|
                            ,object-sub-property ,object-super-property ,reasoner)
                    :is-tbox-axiom-p t
                    :sub-object-property (find-owl-property object-sub-property)
                    :object-property (find-owl-property object-super-property)))))

(defmethod load-axiom :after ((axiom |OWLObjectSubPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (sub-object-property object-property) axiom
    (register-referenced-object-property object-property ontology)
    (register-referenced-object-property sub-object-property ontology)))

;;;
;;;
;;;

(defaxiom |OWLDataSubPropertyAxiom| (|OWLDataPropertyAxiom|) 
   ((sub-data-property :accessor sub-data-property :initform nil :initarg :sub-data-property))) 

(defmethod axioms-equal-p and ((ax1 |OWLDataSubPropertyAxiom|) (ax2 |OWLDataSubPropertyAxiom|))
  (default-slots-equal-p ax1 ax2 '(sub-data-property)))

;; public OWLDataSubPropertyAxiomImpl(OWLDataFactory dataFactory, OWLDataPropertyExpression subProperty,
;;                                       OWLDataPropertyExpression superProperty) {

(defconstructor |OWLAPI-getOWLDataSubPropertyAxiom| (data-sub-property data-super-property)

  (clear-entities data-sub-property data-super-property)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLDataSubPropertyAxiom|
                    :told `(|OWLAPI-getOWLDataSubPropertyAxiom| 
                            ,data-sub-property ,data-super-property ,reasoner)
                    :is-tbox-axiom-p t
                    :sub-data-property (find-owl-property data-sub-property)
                    :data-property (find-owl-property data-super-property)))))


(defmethod load-axiom :after ((axiom |OWLDataSubPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (sub-data-property data-property) axiom

    (register-referenced-data-property sub-data-property ontology)
    (register-referenced-data-property data-property ontology)))



;; public OWLObjectPropertyDomainAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property, OWLDescription domain) {

;; public OWLDataPropertyDomainAxiomImpl(OWLDataFactory dataFactory, OWLDataPropertyExpression property, OWLDescription domain) {

(defaxiom |OWLObjectPropertyDomainAxiom| (|OWLObjectPropertyAxiom|) 
  ((object-property-domain :accessor object-property-domain :initform nil :initarg :object-property-domain)))

(defmethod axioms-equal-p and ((ax1 |OWLObjectPropertyDomainAxiom|) (ax2 |OWLObjectPropertyDomainAxiom|))
  (concept-slots-equal-p ax1 ax2 '(object-property-domain)))

(defaxiom |OWLDataPropertyDomainAxiom| (|OWLDataPropertyAxiom|) 
  ((data-property-domain :accessor data-property-domain :initform nil :initarg :data-property-domain)))

(defmethod axioms-equal-p and ((ax1 |OWLDataPropertyDomainAxiom|) (ax2 |OWLDataPropertyDomainAxiom|))
  (concept-slots-equal-p ax1 ax2 '(data-property-domain)))


;;;
;;;
;;; 

(defconstructor |OWLAPI-getOWLDataPropertyDomainAxiom| (data-property data-property-domain)

  (clear-entities data-property data-property-domain)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLDataPropertyDomainAxiom|
                    :told `(|OWLAPI-getOWLDataPropertyDomainAxiom| 
                            ,data-property ,data-property-domain ,reasoner)
                    :is-tbox-axiom-p t
                    :data-property (find-owl-property data-property)
                    :data-property-domain (find-owl-class data-property-domain)))))

(defmethod load-axiom :after ((axiom |OWLDataPropertyDomainAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-property data-property-domain) axiom
    (register-referenced-data-property data-property ontology)
    (register-referenced-concept data-property-domain ontology)))


;;;
;;;
;;; 

(defconstructor |OWLAPI-getOWLObjectPropertyDomainAxiom| (object-property object-property-domain)

  (clear-entities object-property object-property-domain)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLObjectPropertyDomainAxiom|
                    :told `(|OWLAPI-getOWLObjectPropertyDomainAxiom| 
                            ,object-property ,object-property-domain ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)
                    :object-property-domain (find-owl-class object-property-domain)))))


(defmethod load-axiom :after ((axiom |OWLObjectPropertyDomainAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property object-property-domain) axiom
    (register-referenced-object-property object-property ontology)
    (register-referenced-concept object-property-domain ontology)))


;;;
;;;
;;; 

;; public OWLObjectPropertyRangeAxiomImpl(OWLDataFactory dataFactory, OWLObjectPropertyExpression property, OWLDescription range) {

(defaxiom |OWLObjectPropertyRangeAxiom| (|OWLObjectPropertyAxiom|) 
  ((object-property-range :accessor object-property-range :initform nil :initarg :Object-property-range)))

(defmethod axioms-equal-p and ((ax1 |OWLObjectPropertyRangeAxiom|) (ax2 |OWLObjectPropertyRangeAxiom|))
  (concept-slots-equal-p ax1 ax2 '(object-property-range)))
     
;; public OWLDataPropertyRangeAxiomImpl(OWLDataFactory dataFactory, OWLDataPropertyExpression property, OWLDataRange range) {

(defaxiom |OWLDataPropertyRangeAxiom| (|OWLDataPropertyAxiom|) 
  ((data-property-range :accessor data-property-range :initform nil :initarg :data-property-range)))

(defmethod axioms-equal-p and ((ax1 |OWLDataPropertyRangeAxiom|) (ax2 |OWLDataPropertyRangeAxiom|))
  (datarange-slots-equal-p ax1 ax2 '(data-property-range)))
     
;;;
;;;
;;;

(defconstructor |OWLAPI-getOWLDataPropertyRangeAxiom| (data-property data-range)

  (clear-entities data-property data-range)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLDataPropertyRangeAxiom|
                    :told `(|OWLAPI-getOWLDataPropertyRangeAxiom| ,data-property ,data-range ,reasoner)
                    :is-tbox-axiom-p t
                    :data-property (find-owl-property data-property)
                    :data-property-range (find-owl-class data-range)))))

(defmethod load-axiom :after ((axiom |OWLDataPropertyRangeAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-property data-property-range) axiom
    (register-referenced-data-property data-property ontology)
    (register-referenced-datarange data-property-range ontology)))


;;;
;;;
;;; 

(defconstructor |OWLAPI-getOWLObjectPropertyRangeAxiom| (object-property object-property-range)

  (clear-entities object-property object-property-range)

  (with-reasoner (reasoner)
    (return-policy-new-axiom 
     (make-instance '|OWLObjectPropertyRangeAxiom|
                    :told `(|OWLAPI-getOWLObjectPropertyRangeAxiom| 
                            ,object-property ,object-property-range ,reasoner)
                    :is-tbox-axiom-p t
                    :object-property (find-owl-property object-property)
                    :object-property-range (find-owl-class object-property-range)))))


(defmethod load-axiom :after ((axiom |OWLObjectPropertyRangeAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property object-property-range) axiom
    (register-referenced-object-property object-property ontology)
    (register-referenced-concept object-property-range ontology)))


;;;
;;; 
;;; 

(defpersistentclass |OWLOntologyChange| () ;; abstrakt
  ((ontology :accessor change-ontology :initform nil :initarg :ontology)
   (told :accessor change-told :initform nil :initarg :told)))

(defmethod print-object ((ax |OWLOntologyChange|) stream)
  (format stream "~S" (change-told ax)))

(defmethod unparse ((ax |OWLOntologyChange|))
  (change-told ax))

(defmethod initialize-instance :after ((axiom |OWLOntologyChange|) &rest initargs)
  (declare (ignorable initargs))

  (cond ((or (auto-apply *cur-reasoner*)
	     ;; (ont-loaded-p (ontology axiom))
             )
         (apply-change axiom))
        (t (push axiom (changes *cur-reasoner*)))))

(owlapi-defun (|OWLAPI-clearChanges|) 
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (changes *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))


(owlapi-defun (|OWLAPI-getChanges|) 
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (last-answer *cur-reasoner*)
          (changes *cur-reasoner*))
    (return-by-policy)))


(owlapi-defun (|OWLAPI-autoApplyChanges|)
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (auto-apply *cur-reasoner*) t)
    (clear-last-answer)
    (return-by-policy)))
    

(owlapi-defun (|OWLAPI-manuallyApplyChanges|)
  (&optional reasoner)
  (with-reasoner (reasoner)
    (setf (auto-apply *cur-reasoner*) nil)
    (clear-last-answer)
    (return-by-policy)))

;;;
;;; Change Interface
;;; 

(owlapi-defun (|OWLAPI-applyChanges|)
  (&optional reasoner)
  (with-reasoner (reasoner)
    (dolist (change (reverse (changes *cur-reasoner*)))
      (apply-change change))

    (|OWLAPI-clearChanges|)))
           
;;;
;;;
;;; 

(defpersistentclass |OWLAxiomChange| (|OWLOntologyChange|) ;; abstrakt 
  ((axiom :accessor axiom :initform nil :initarg :axiom)))

;;  public OWLAxiomChange(OWLOntology ont, OWLAxiom axiom) {

;;;
;;;
;;;

(defpersistentclass |AddAxiom| (|OWLAxiomChange|) ())

(defmethod apply-change ((change |AddAxiom|))
  (let* ((ont (change-ontology change))
         (axiom (axiom change))
         (axiom 
               
          (or (find-owl-axiom axiom ont)
              ;;; Constructor Call to identify the axiom by value: 
              ;;; not found? try with reasoner added...
              (find-owl-axiom (append axiom (list (owlapi-reasoner-name *cur-reasoner*))) ont)
               
              (let ((old-policy (return-policy *cur-reasoner*))
                    (old-mode (auto-mode *cur-reasoner*)))
                                
                (unwind-protect
                    (progn 
                      (setf (return-policy *cur-reasoner*) nil
                            (auto-mode *cur-reasoner*) nil)
                                      
                      (cond ((consp axiom)
                             (if (fboundp (first axiom))
                                 (apply (symbol-function (first axiom))
                                        (rest axiom))
                               (|OWLAPI-parse| axiom)))
                            ((stringp axiom)
                             (|OWLAPI-parseNative| axiom))
                            (t (owlapi-parser-error "Bad axiom: ~A" axiom))))
                                          
                  (setf (return-policy *cur-reasoner*) old-policy
                        (auto-mode *cur-reasoner*) old-mode))))))

    (add-axiom ont axiom)))

;; public AddAxiom(OWLOntology ont, OWLAxiom axiom) {

(owlapi-defun (|OWLAPI-addAxiom|) (&rest args)
  (apply #'|OWLAPI-AddAxiom| args))

(owlapi-defun (|OWLAPI-AddAxiom|) (ont axiom &optional reasoner)
  (with-reasoner (reasoner)
    (make-instance '|AddAxiom|
                   :told `(|OWLAPI-AddAxiom| ,ont ,axiom ,reasoner)
                   :axiom axiom
                   :ontology (find-owl-ontology ont))

    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-addAxioms|) (&rest args)
  (apply #'|OWLAPI-AddAxioms| args))

(owlapi-defun (|OWLAPI-AddAxioms|) (ont axioms &optional reasoner)
  (with-reasoner (reasoner)
    (dolist (axiom axioms)
      (|OWLAPI-addAxiom| ont axiom reasoner))
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;;

(defpersistentclass |RemoveAxiom| (|OWLAxiomChange|) ())

(defmethod apply-change ((change |RemoveAxiom|))
  (let* ((ont (change-ontology change))
         (axiom (axiom change))
         (axiom (or (find-owl-axiom axiom ont)
                    (find-owl-axiom (append axiom (list (owlapi-reasoner-name *cur-reasoner*))) ont)
                    (owlapi-runtime-error "Can't find axiom ~S in ~A" axiom ont))))
                   
    (remove-axiom ont axiom)))

;;  public RemoveAxiom(OWLOntology ont, OWLAxiom axiom) {

(owlapi-defun (|OWLAPI-removeAxiom|) (&rest args)
  (apply #'|OWLAPI-RemoveAxiom| args))

(owlapi-defun (|OWLAPI-RemoveAxiom|) (ont axiom &optional reasoner)
  (with-reasoner (reasoner)
    (make-instance '|RemoveAxiom|
                   :told `(|OWLAPI-RemoveAxiom| ,ont ,axiom ,reasoner)
                   :axiom axiom
                   :ontology (find-owl-ontology ont))

    (clear-last-answer)
    (return-by-policy)))

(owlapi-defun (|OWLAPI-removeAxioms|) (&rest args)
  (apply #'|OWLAPI-RemoveAxioms| args))

(owlapi-defun (|OWLAPI-RemoveAxioms|) (ont axioms &optional reasoner)
  (with-reasoner (reasoner)
    (dolist (axiom axioms)
      (|OWLAPI-removeAxiom| ont axiom reasoner))
    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;;

(defmethod register-additional-ontology-name ((ont symbol) new-name &optional reasoner)
  (register-additional-ontology-name (find-owl-ontology ont) new-name reasoner))

(defmethod register-additional-ontology-name ((ont string) new-name &optional reasoner)
  (register-additional-ontology-name (find-owl-ontology ont) new-name reasoner))

(defmethod register-additional-ontology-name ((ont ontology) new-name &optional reasoner)
  (with-reasoner (reasoner)
    (let* ((old-name (name ont))
           (ont2 (gethash new-name (ontologies *cur-reasoner*))))
    
      (unless (equal old-name new-name)
        (cond (ont2
               (error "Cannot change name of ~A to ~A, since ontology ~A already exists!"
                      old-name new-name new-name))
              (t
               (pushnew new-name (all-names ont))
               ;;; (setf (name ont) new-name)
               (setf (gethash new-name (ontologies *cur-reasoner*)) ont)))))))

;;;
;;;
;;;

(defmethod set-primary-ontology-name ((ont symbol) new-name &optional reasoner)
  (set-primary-ontology-name (find-owl-ontology ont) new-name reasoner))

(defmethod set-primary-ontology-name ((ont string) new-name &optional reasoner)
  (set-primary-ontology-name (find-owl-ontology ont) new-name reasoner))

(defmethod set-primary-ontology-name ((ont ontology) new-name &optional reasoner)
  (with-reasoner (reasoner)
    (let* ((old-name (name ont))
           (ont2 (gethash new-name (ontologies *cur-reasoner*))))

      (unless (equalp old-name new-name)
        (cond ((eq ont ont2)
               )
              (ont2
               (error "Cannot change primary name of ~A to ~A, since ontology ~A already exists!"
                      old-name new-name new-name))
              (t
               (pushnew new-name (all-names ont))
               (setf (name ont) new-name)
               (setf (gethash new-name (ontologies *cur-reasoner*)) ont)))))))

;;;
;;;
;;;

(defpersistentclass |SetOntologyURI| (|OWLOntologyChange|) 
  ((uri :accessor change-uri :initform nil :initarg :uri)))

(defmethod apply-change ((change |SetOntologyURI|))
  (let* ((ont (find-owl-ontology (change-ontology change)))
         (new-name (change-uri change)))
    (register-additional-ontology-name ont new-name)))

;;  public SetOntologyURI(OWLOntology ont, URI newURI) {

(owlapi-defun (|OWLAPI-SetOntologyURI|) (ont uri &optional reasoner)
  (with-reasoner (reasoner)
    (make-instance '|SetOntologyURI|
                   :told `(|OWLAPI-SetOntologyURI| ,ont ,uri ,reasoner)
                   :uri (find-owl-entity uri)
                   :ontology (find-owl-ontology ont))

    (clear-last-answer)
    (return-by-policy)))

;;;
;;;
;;; 

(defun export-uris (stream)
  (let ((prefixes 
         (reasoner-get-prefixes)))
    (loop as (prefix uri) in prefixes do
          (when prefix
            (pprint `(define-prefix ,prefix ,uri)
                    stream)))
    (loop as (prefix uri) in prefixes do
          (unless prefix
            (pprint `(define-prefix ,prefix ,uri)
                    stream)))))

  
(owlapi-defun (|OWLAPI-exportOntology|) (ontology fn 
						&key
						reasoner
						(syntax :native)
						(quoted nil)
						(init t)
						(header t))

  (declare (ignorable syntax))
	    
  (with-reasoner (reasoner)
	      
    (labels ((quote1 (form)
               (if quoted
                   `(quote ,form)
                 form)))     
	      
      (with-open-file (stream fn
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists (if header 
                                             :supersede
                                           :append))
		
        (with-reasoner (reasoner)
          (let ((ont (find-owl-ontology ontology)))
            (clear-last-answer)

            (when header
              (pprint `(|OWLAPI-newReasoner| 
                        ,(quote1 (owlapi-reasoner-name *cur-reasoner*))
                        nil ,init)
                      stream)
              (terpri stream)
	      
	      (export-uris stream)
	      (terpri stream))

            (pprint `(|OWLAPI-newOntology| ,(quote1 ontology) 
                                           ,(quote1 (owlapi-reasoner-name *cur-reasoner*)))
                    stream)
            (terpri stream)
		      
            (when header
              (pprint `(|OWLAPI-loadOntology| ,(quote1 ontology) 
                                              ,(quote1 (owlapi-reasoner-name *cur-reasoner*)))
                      stream)
              (terpri stream))
		    
            (pprint `(|OWLAPI-autoAddAxiomsTo| ,(quote1 ontology) 
                                               ,(quote1 (owlapi-reasoner-name *cur-reasoner*)))
                    stream)
            (terpri stream)
		    
            (dolist (ax (reverse (axioms ont)))
              (let ((ax (told ax)))
                (pprint `(,(first ax)
                          ,@(mapcar #'quote1 (rest ax)))
                        stream)
                (terpri stream))))))))
			      
  (return-by-policy))


(owlapi-defun (|OWLAPI-exportReasoner|) (reasoner fn 
						&key
						(syntax :native)
						(quoted nil)
						(init t))
	    
  (labels ((quote1 (form)
             (if quoted
                 `(quote ,form)
               form)))
	      
    (with-reasoner (reasoner)
	      
      (with-open-file (stream fn
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede)
		
        (clear-last-answer)
		  
        (pprint `(|OWLAPI-newReasoner| 
                  ,(quote1 (owlapi-reasoner-name *cur-reasoner*))
                  nil ,init)
                stream)
        (terpri stream)
	
	(export-uris stream)
	(terpri stream)))
    
    (dolist (name (get-ontology-names *cur-reasoner*))
      (|OWLAPI-exportOntology| name  fn
                               :reasoner reasoner
                               :header nil
                               :init nil
                               :quoted quoted
                               :syntax syntax))
	      
    (return-by-policy)))
