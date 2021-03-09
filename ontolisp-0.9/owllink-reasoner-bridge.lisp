;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

(in-package :owl-syntaxes)

;;;
;;;;  owllink-rasoner-bridge.lisp
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
;;;   Purpose: Example of a concrete OWLlink reasoner bridge (for RacerPro). 
;;;            Supplied for illustration purposes only. 
;;; 

(defun reduced-object-property-synset2 (role-nameset &optional up-p) 
  (if (not up-p)
      (cond ((reasoner-top-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             (let ((res (without-top-object-property role-nameset *owllink-kb-and-reasoner*)))
               (cons owlapi:+owlapi-owl-top-object-role+ res)))
            ((reasoner-bottom-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             nil)
            (t role-nameset))
    
    (cond ((reasoner-top-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           nil)
          ((reasoner-bottom-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           (let ((res (without-bottom-object-property role-nameset *owllink-kb-and-reasoner*)))
             (cons owlapi:+owlapi-owl-bottom-object-role+ res)))
          (t role-nameset))))


(defun reduced-data-property-synset2 (role-nameset &optional up-p) 
  (if (not up-p)
      (cond ((reasoner-top-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             (let ((res (without-top-data-property role-nameset *owllink-kb-and-reasoner*)))
               (cons owlapi:+owlapi-owl-top-data-role+ res)))
            ((reasoner-bottom-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             nil)
            (t role-nameset))
    
    (cond ((reasoner-top-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           nil)
          ((reasoner-bottom-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           (let ((res (without-bottom-data-property role-nameset *owllink-kb-and-reasoner*)))
             (cons owlapi:+owlapi-owl-bottom-data-role+ res)))
          (t role-nameset))))


(defun reduced-synset2 (concept-nameset &optional up-p) 
  (if (not up-p)
      (cond ((member (first concept-nameset)
                     '(top *top*))
             (let ((res (without-top concept-nameset)))
               (cons owlapi:+owlapi-owl-thing+ res)))
            ((member (first concept-nameset)
                     '(bottom *bottom*))
             nil)
            (t concept-nameset))
    
    (cond ((member (first concept-nameset)
                   '(top *top*))
             nil)
          ((member (first concept-nameset)
                   '(bottom *bottom*))
           (let ((res (without-bottom concept-nameset)))
             (cons owlapi:+owlapi-owl-nothing+ res)))
          (t concept-nameset))))

(defun reduced-synset (concept-nameset &optional up-p)
  (declare (ignorable up-p))
           
  (cond ((member (first concept-nameset)
                 '(top *top*))
         (cons owlapi:+owlapi-owl-thing+ 
               (without-top concept-nameset)))
        ((member (first concept-nameset)
                 '(bottom *bottom*))
         (cons owlapi:+owlapi-owl-nothing+ 
               (without-bottom concept-nameset)))
        (t concept-nameset)))

(defun concept-synset (concept-nameset &optional up-p)
  (make-synset '|Class| (reduced-synset concept-nameset up-p)))

;;;
;;;
;;;

(defun owllink-object-property-p (role)
  (reasoner-object-property-p role *owllink-kb-and-reasoner*))

(defun owllink-data-property-p (role)
  (reasoner-data-property-p role *owllink-kb-and-reasoner*))

(defun owllink-annotation-property-p (role)
  (reasoner-annotation-property-p role *owllink-kb-and-reasoner*))

;;;
;;;
;;;

(defun owllink-concept-synonyms (concept)
  (reasoner-concept-synonyms concept *owllink-kb-and-reasoner*))
         
(defun owllink-bottom-synonyms ()
  (let ((x 
         (without-bottom
          (reasoner-concept-synonyms 'bottom *owllink-kb-and-reasoner*))))
    (cons owlapi:+owlapi-owl-nothing+ x)))

;;;
;;;
;;;

(defun owllink-get-disjoint-classes (concept &optional flat-p)
  (let ((res
         (reasoner-get-disjoint-concepts concept *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (concept-list-result res)
      (flat-concept-list-result res))))

(defun owllink-get-sub-classes (concept &optional flat-p)
  (let ((res
         (if *direct*
             (reasoner-concept-children concept *owllink-kb-and-reasoner*)
           (remove-concept-by-synset 
            concept
            (reasoner-concept-descendants concept *owllink-kb-and-reasoner*)))))
    (if (not flat-p)
        (concept-list-result res nil t)
      (flat-concept-list-result res))))

(defun owllink-get-super-classes (concept &optional flat-p)
  (let ((res
         (if *direct*
             (reasoner-concept-parents
              concept *owllink-kb-and-reasoner*)
           (remove-concept-by-synset
            concept
            (reasoner-concept-ancestors concept *owllink-kb-and-reasoner*)))))
    (if (not flat-p)
        (concept-list-result res nil t)
      (flat-concept-list-result res))))

(defun owllink-class-hierarchy2 (concept &optional up-p)
  (let ((kb *owllink-kb-and-reasoner*))
    
    (make-owllink-message
     
     '|ClassHierarchy| 
     
     nil
     
     (cons 
      
      (make-synset '|Class| 
                   (owllink-bottom-synonyms))

      (loop for concept-nameset in
              
            (cons (reasoner-atomic-concept-synonyms concept kb)
                  (if (not up-p)
                      (reasoner-concept-descendants concept kb)
                    (reasoner-concept-ancestors concept kb)))

            as classes = 

            (when concept-nameset
              (if (not up-p)
                  (reasoner-concept-children
                   (first concept-nameset) kb)
                (reasoner-concept-parents
                 (first concept-nameset) kb)))

            as classes1 =  (mapcar #'(lambda (x) 
                                       (reduced-synset2 x up-p))
                                   classes)
            
            when (and classes1 
                      ;; not all NIL? 
                      (some #'identity classes1))
              
            collect (make-owllink-message
                     (if (not up-p)
                         '|ClassSubClassesPair|
                       '|ClassSuperClassesPair|)

                     nil

                     (list (concept-synset concept-nameset up-p)

                           (make-set-of-synsets '|SubClass|
                                                (remove nil 
                                                        (mapcar #'(lambda (x) 
                                                                    (make-synset '|Class| x))
                                                                classes1))))))))))

(defun owllink-roles-equivalent-p (role-1 role-2)
  (reasoner-role-equivalent-p role-1 role-2 *owllink-kb-and-reasoner*))

(defun owllink-roles-disjoint-p (role-1 role-2) 
  (reasoner-role-disjoint-p role-1 role-2 *owllink-kb-and-reasoner*))

(defun owllink-role-satisfiable-p (role) 
  (reasoner-role-satisfiable-p role *owllink-kb-and-reasoner*))

(defun owllink-role-functional-p (role) 
  (reasoner-role-functional-p role *owllink-kb-and-reasoner*))

(defun owllink-role-inverse-functional-p (role) 
  (reasoner-role-inverse-functional-p role *owllink-kb-and-reasoner*))

(defun owllink-role-reflexive-p (role)
  (reasoner-role-reflexive-p role *owllink-kb-and-reasoner*))

(defun owllink-role-irreflexive-p (role) 
  (reasoner-role-irreflexive-p role *owllink-kb-and-reasoner*))

(defun owllink-role-symmetric-p (role) 
  (reasoner-role-symmetric-p role *owllink-kb-and-reasoner*))

(defun owllink-role-asymmetric-p (role) 
  (reasoner-role-asymmetric-p role *owllink-kb-and-reasoner*))

(defun owllink-role-transitive-p (role) 
  (reasoner-role-transitive-p role *owllink-kb-and-reasoner*))

(defun owllink-role-subsumes-p (role-1 role-2) 
  (reasoner-role-subsumes-p role-1 role-2 *owllink-kb-and-reasoner*))

(defun owllink-object-property-synonyms (role &optional (no-internal-roles-p t))
  (when (owllink-object-property-p role)
    (cond ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-bottom-object-role+
                             (unless no-internal-roles-p 
                               (reasoner-get-object-bottom-role *owllink-kb-and-reasoner*))))))

          ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-top-object-role+
                             (unless no-internal-roles-p 
                               (reasoner-get-object-top-role *owllink-kb-and-reasoner*))))))
        
          (t 
           (reasoner-only-object-properties
            (remove-duplicates
             (cons role (reasoner-equivalent-roles role *owllink-kb-and-reasoner*)))
            *owllink-kb-and-reasoner*)))))

(defun owllink-role-children (role)
  (when role
    (cond ((owllink-object-property-p role)
           (cond ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                  (or
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-top-object-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-bottom-object-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-data-properties
                                              (reasoner-atomic-role-parents 
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not ; haengt nicht unter TOP-ROLE -> raus
                                         (member 
                                          (reasoner-get-object-top-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-object-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-bottom-object-role+)))
                 ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or 
                   (reasoner-only-object-properties
                    (reasoner-atomic-role-children role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-bottom-object-role+)))))

          ((owllink-data-property-p role)
           (cond ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                  (or
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-top-data-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-bottom-data-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-object-properties
                                              (reasoner-atomic-role-parents 
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not 
                                         (member 
                                          (reasoner-get-datatype-top-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-data-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-bottom-data-role+)))
                 ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or 
                   (reasoner-only-data-properties 
                    (reasoner-atomic-role-children role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-bottom-data-role+)))))

          ((owllink-annotation-property-p role)
           (reasoner-only-annotation-properties 
            (reasoner-atomic-role-children role *owllink-kb-and-reasoner*)
            *owllink-kb-and-reasoner*))

          (t ; (inv objectproperty) etc. 
           nil))))

(defun owllink-role-parents (role)
  (when role 
    (cond ((owllink-object-property-p role)
           (cond ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                  (or 
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-bottom-object-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-top-object-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-data-properties
                                              (reasoner-atomic-role-children
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not 
                                         (member 
                                          (reasoner-get-object-bottom-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-object-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-top-object-role+)))
                 ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or 
                   (reasoner-only-object-properties
                    (reasoner-atomic-role-parents 
                     role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-top-object-role+)))))

          ((owllink-data-property-p role)
           (cond ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                  (or 
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-bottom-data-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-top-data-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-object-properties
                                              (reasoner-atomic-role-children
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not 
                                         (member
                                          (reasoner-get-datatype-bottom-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-data-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-top-data-role+)))
                 ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or
                   (reasoner-only-data-properties 
                    (reasoner-atomic-role-parents role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-top-data-role+)))))

          ((owllink-annotation-property-p role)
           (reasoner-only-annotation-properties 
            (reasoner-atomic-role-parents role *owllink-kb-and-reasoner*)
            *owllink-kb-and-reasoner*))

          (t ; (inv objectproperty) etc. 
           nil))))

(defun owllink-role-descendants (role)
  (cond ((owllink-object-property-p role)
         (cond ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset 
                 role
                 (reasoner-get-object-properties *owllink-kb-and-reasoner*)))
               (t 
                (cons owlapi:+owlapi-owl-bottom-object-role+
                      (remove-role-by-synset role
                              (reasoner-only-object-properties
                               (reasoner-atomic-role-descendants 
                                role *owllink-kb-and-reasoner*)
                               *owllink-kb-and-reasoner*))))))

        ((owllink-data-property-p role)
         (cond ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset role
                        (reasoner-get-data-properties *owllink-kb-and-reasoner*)))
                (t 
                 (cons owlapi:+owlapi-owl-bottom-data-role+
                       (remove-role-by-synset role 
                               (reasoner-only-data-properties
                                (reasoner-atomic-role-descendants 
                                 role *owllink-kb-and-reasoner*)
                                *owllink-kb-and-reasoner*))))))

        ((owllink-annotation-property-p role)
         (remove-role-by-synset role 
                 (reasoner-only-annotation-properties 
                  (reasoner-atomic-role-descendants
                   role *owllink-kb-and-reasoner*)
                  *owllink-kb-and-reasoner*)))

        (t ; (inv objectproperty) etc. 
         nil)))

(defun owllink-role-ancestors (role)
  (cond ((owllink-object-property-p role)
         (cond ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset role
                        (reasoner-get-object-properties *owllink-kb-and-reasoner*)))
               (t 
                (cons owlapi:+owlapi-owl-top-object-role+
                      (remove-role-by-synset role 
                              (reasoner-only-object-properties
                               (reasoner-atomic-role-ancestors
                                role *owllink-kb-and-reasoner*)
                               *owllink-kb-and-reasoner*))))))

        ((owllink-data-property-p role)
         (cond ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset role
                        (reasoner-get-data-properties *owllink-kb-and-reasoner*)))
               (t 
                (cons owlapi:+owlapi-owl-top-data-role+
                      (remove-role-by-synset role 
                              (reasoner-only-data-properties
                               (reasoner-atomic-role-ancestors
                                role *owllink-kb-and-reasoner*)
                               *owllink-kb-and-reasoner*))))))

        ((owllink-annotation-property-p role)
         (remove-role-by-synset role 
                 (reasoner-only-annotation-properties 
                  (reasoner-atomic-role-ancestors
                   role *owllink-kb-and-reasoner*)
                  *owllink-kb-and-reasoner*)))

        (t ; (inv dataproperty) etc. 
         nil)))

(defun owllink-get-disjoint-object-properties (role &optional flat-p)
  (let ((res
         (reasoner-get-disjoint-object-properties role *owllink-kb-and-reasoner*)))
  (if (not flat-p)
      (object-property-list-result res nil nil t)
    (flat-object-property-list-result res))))

(defun owllink-object-property-hierarchy2 (role &optional up-p)
  (make-owllink-message 
   '|ObjectPropertyHierarchy|

   nil

   (cons 
      
    (make-synset '|ObjectProperty| 
                 (reasoner-only-object-properties
                  (owllink-object-property-synonyms owlapi:+owlapi-owl-bottom-object-role+)
                  *owllink-kb-and-reasoner*))

    (loop for role-set in
              
          (remove-duplicates
           (mapcar #'owllink-object-property-synonyms 
                   (cons role
                         (if (not up-p)
                             (owllink-role-descendants role)
                           (owllink-role-ancestors role))))
           :test #'owlapi:set-equal)

          as roles = 
          
          (remove nil 
                  (remove-duplicates
                   (mapcar #'owllink-object-property-synonyms
                           (when role
                             (if (not up-p)
                                 (owllink-role-children (first role-set))
                               (owllink-role-parents (first role-set)))))
                   :test #'equal))
          
          as roles1 =  (mapcar #'(lambda (x) 
                                   (reduced-object-property-synset2 x up-p))
                               roles)

          when (and roles1 
                    ;; not all NIL? 
                    (some #'identity roles1))
              
          collect (make-owllink-message
                   (if (not up-p)
                       '|ObjectPropertySubObjectPropertiesPair|
                     '|ObjectPropertySuperObjectPropertiesPair|)

                   nil

                   (list 

                    (make-synset '|ObjectProperty| role-set)

                    (make-set-of-synsets '|SubObjectProperty|
                                         (remove nil 
                                                 (mapcar #'(lambda (x) 
                                                             (make-synset '|ObjectProperty| x))
                                                         roles1)))))))))

(defun owllink-data-property-synonyms (role &optional (no-internal-roles-p t))
  (when (owllink-data-property-p role)
    (cond ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-bottom-data-role+
                             (unless no-internal-roles-p
                               (reasoner-get-datatype-bottom-role *owllink-kb-and-reasoner*))))))

          ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-top-data-role+
                             #+:ignore
                             (unless no-internal-roles-p 
                               (reasoner-get-datatype-top-role *owllink-kb-and-reasoner*))))))

          (t 
           (reasoner-only-data-properties
            (remove-duplicates
             (cons role (reasoner-equivalent-roles
                         role *owllink-kb-and-reasoner*)))
            *owllink-kb-and-reasoner*)))))

(defun owllink-get-disjoint-data-properties (role &optional flat-p)
  (let ((res
         (reasoner-get-disjoint-data-properties role *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (data-property-list-result res)
      (flat-data-property-list-result res))))

(defun owllink-data-property-hierarchy2 (role &optional up-p)
  (make-owllink-message 
   '|DataPropertyHierarchy|

   nil

   (cons 
      
    (make-synset '|DataProperty| 
                 (reasoner-only-data-properties
                  (owllink-data-property-synonyms owlapi:+owlapi-owl-bottom-data-role+)
                  *owllink-kb-and-reasoner*))

    (loop for role-set in
              
          (remove-duplicates
           (mapcar #'owllink-data-property-synonyms 
                   (cons role
                         (if (not up-p)
                             (owllink-role-descendants role)
                           (owllink-role-ancestors role))))
           :test #'owlapi:set-equal)

          as roles = 

          (remove nil 
                  (remove-duplicates
                   (mapcar #'owllink-data-property-synonyms
                           (when role
                             (if (not up-p)
                                 (owllink-role-children (first role-set))
                               (owllink-role-parents (first role-set)))))
                   :test #'equal))
          
          as roles1 =  (mapcar #'(lambda (x) 
                                   (reduced-data-property-synset2 x up-p))
                               roles)

          when (and roles1 
                    ;; not all NIL? 
                    (some #'identity roles1))

          collect (make-owllink-message
                   (if (not up-p)
                       '|DataPropertySubDataPropertiesPair|
                     '|DataPropertySuperDataPropertiesPair|)

                   nil

                   (list 

                    (make-synset '|DataProperty| role-set)

                    (make-set-of-synsets '|SubDataProperty|
                                         (remove nil 
                                                 (mapcar #'(lambda (x) 
                                                             (make-synset '|DataProperty| x))
                                                         roles1)))))))))

(defun owllink-is-instance-of (ind concept &optional direct-p)
  (reasoner-instance-of-p ind concept *owllink-kb-and-reasoner* direct-p))

(defun owllink-get-types (ind &optional flat-p)
  (let ((res 
         (reasoner-get-types ind *owllink-kb-and-reasoner* *direct*)))
    (if (not flat-p)
        (concept-list-result res)
      (flat-concept-list-result res))))

(defun owllink-get-disjoint-individuals (ind &optional flat-p)
  (let* (;; #+:racer-server
         ;; (racer::*optimize-datatype-role-fillers* nil) ; RACER BUG! behoben? 
         (res
          (reasoner-individual-antonyms ind *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-object-properties-between (source target &optional flat-p)
  (let* ((res 
          (reasoner-retrieve-individual-filled-roles
           source target *owllink-kb-and-reasoner*
           :roles (without-top-and-bottom-object-properties
                   (reasoner-get-object-properties *owllink-kb-and-reasoner*)
                   *owllink-kb-and-reasoner*)
           :negated-p *negative*
           :no-inverses-p t))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-object-role+ res)
            (cons owlapi:+owlapi-owl-top-object-role+ res))))
    (if (not flat-p)
        (object-property-list-result res nil nil t)
      (flat-object-property-list-result res))))

(defun owllink-get-object-properties-of-source (source &optional flat-p)
  (let* ((res 
          (mapcar #'first 
                  (reasoner-get-individual-successors 
                   source *owllink-kb-and-reasoner*
                   :roles (without-top-and-bottom-object-properties 
                           (reasoner-get-object-properties *owllink-kb-and-reasoner*)
                           *owllink-kb-and-reasoner*)
                   :negated-p *negative*
                   :no-inverses-p t)))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-object-role+ res)
            (cons owlapi:+owlapi-owl-top-object-role+ res))))
    (if (not flat-p)
        (object-property-list-result res nil nil t)
      (flat-object-property-list-result res))))

(defun owllink-get-object-properties-of-target (target &optional flat-p)
  (let* ((res 
          (delete nil
                  (mapcar #'(lambda (x)
                              (if (consp (first x))
                                  (second (first x))
                                (first x)))
                          (reasoner-get-individual-successors 
                           target *owllink-kb-and-reasoner*
                           :negated-p *negative*
                           :no-inverses-p nil
                           :roles (without-top-and-bottom-object-properties
                                   (reasoner-get-object-properties
                                    *owllink-kb-and-reasoner*)
                                   *owllink-kb-and-reasoner*)
                           :only-inverses-p t))))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-object-role+ res)
            (cons owlapi:+owlapi-owl-top-object-role+ res))))
    (if (not flat-p)
        (object-property-list-result res nil nil t)
      (flat-object-property-list-result res))))
      
(defun owllink-get-data-properties-between (source literal &optional flat-p)
  (let* ((res 
          (loop for (role literals) in
                (reasoner-get-individual-datatype-fillers source *owllink-kb-and-reasoner*)
                when (find literal literals :test #'equal)
                collect role))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-data-role+ res)
            (cons owlapi:+owlapi-owl-top-data-role+ res))))
    (if (not flat-p)
        (data-property-list-result res nil nil t)
      (flat-data-property-list-result res))))

(defun owllink-get-data-properties-of-source (source &optional flat-p)
  (let* ((res 
          (mapcar #'first 
                  (reasoner-get-individual-datatype-fillers source *owllink-kb-and-reasoner*)))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-data-role+ res)
            (cons owlapi:+owlapi-owl-top-data-role+ res))))
    (if (not flat-p)
        (data-property-list-result res nil nil t)
      (flat-data-property-list-result res))))

(defun owllink-get-instances (concept direct-p &optional flat-p)
  (let ((res 
         (reasoner-get-instances 
          concept *owllink-kb-and-reasoner* direct-p)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-object-property-targets (ind role &optional flat-p)
  (let ((res
         (reasoner-retrieve-individual-fillers 
          ind 
          (if *negative*
              `(not ,role)
            role)
          *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-object-property-sources (ind role &optional flat-p)
  (let ((res
         (reasoner-retrieve-individual-predecessors
          ind (if *negative*
                  `(not ,role)
                role)
          *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-data-property-targets (ind property)
  (flat-literal-list-result 
   (mapcar #'(lambda (x) 
               `(|Literal| nil ,x))
           (reasoner-retrieve-individual-told-datatype-fillers 
            ind property *owllink-kb-and-reasoner*))))

(defun owllink-individual-p (ind)
  (symbolp ind))

(defun owllink-get-individuals ()
  (all-individuals *owllink-kb-and-reasoner*))

(defun owllink-get-data-properties-of-literal (literal &optional flat-p)
  (racer-prepare-substrate :abox *owllink-kb-and-reasoner* :prepare-now-p t)
              
  (let* ((res
          (let ((hash (ts::datatype-property-values-and-types-cache *cur-substrate*))
                (res nil))
            (maphash #'(lambda (key vals)
                         (declare (ignorable key))
                         (dolist (x vals)
                           #+:ignore
                           (pprint (list literal 
                                         (third (first x)) 
                                         (equal (third (first x)) 
                                                literal)))
                           (when (and (equal (third (first x)) 
                                             literal)
                                      (not (second x))) ; no annotation 
                             (push (caar x)
                                   res))))
                     hash)
            res))
         (res 
          (remove-duplicates 
           (cons owlapi:+owlapi-owl-top-data-role+
                 (append res
                         (apply #'append
                                (mapcar #'owllink-role-ancestors res)))))))
    (if (not flat-p)
        (data-property-list-result res nil nil t)
      (flat-data-property-list-result res))))

(defun owllink-get-data-property-sources (literal property &optional flat-p)
  (racer-prepare-substrate :abox *owllink-kb-and-reasoner* :prepare-now-p t)
              
  (let ((res
         (let ((hash (ts::datatype-property-values-and-types-cache *cur-substrate*))
               (res nil))
                  (maphash #'(lambda (key vals)
                               (when (some #'(lambda (x)
                                               (and (equal property (first (first x)))
                                                    (equal literal  (third (first x)) )
                                                    (not (second x)))) ; no annotation 
                                           vals)
                                 (push key res)))
                           hash)
                  res)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

;;;
;;;
;;;

(defmethod owllink-entailed-p (axiom)
  (owlapi:entailed-p axiom))

(defmethod owllink-direct-entailed-p (axiom)
  (owlapi:direct-entailed-p axiom))

