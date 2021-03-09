;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

(in-package :owl-syntaxes)

;;;
;;;;  owllink-parsers.lisp
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
;;;   Purpose: Parser macros for the OWLlink functional processor. 
;;;            Deliberately not very hygienic ;-) Will be improved sometime. 
;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-parser-name (type)
    (intern (format nil "owllink-parse-~A" type)
            (find-package :owl-syntaxes))))

(defmacro loop-parser (type attributes-and-binders inner-parser-name before-loop-form 
                             &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))
        
    `(defun ,name (expressions1 &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (when *debug-p*
           (terpri)
           (pprint (list :loop-call ',name expressions1)))

       (if (and (consp (first expressions1))
                (eq (expand-functional-tag-name (first (first expressions1)))
                    ',type))

           (let* ((expressions (first expressions1)))

             (multiple-value-bind (attributes expressions)
                 (apply #'get-attributes expressions ',(first attributes-and-binders) args)
          
               (declare (ignorable attributes expressions))

               ,before-loop-form

               (,@(second attributes-and-binders)

                (let ((done nil)
                      (response nil)
                      (responses nil)
                      (expressions (cddr expressions))) 
                  ;; remove Tag name and attributes, e.g. of Tell  

                  (declare (ignorable done))

                  (loop while ;(and expressions (not done)) do
                         expressions do

                        (setf done t)

                        (when *debug-p*
                          (terpri)
                          (pprint (list :in-loop ',name expressions responses)))
                        
                        (multiple-value-setq (response expressions)

                            (apply (symbol-function ',(get-parser-name inner-parser-name))
                                   expressions args))

                        (unless response (setf expressions (cdr expressions)))

                        (when ;response
                            t
                          (setf done nil)
                          (push response responses)))

                  (setf response (nreverse responses))

                  (multiple-value-bind (response expressions)
                      (progn 
                        ,@validate-and-return-form)

                    (when *debug-p*
                      (terpri)
                      (pprint (list :loop-return ',name response expressions)))
                 
                    (values response (rest expressions1)))))))

         (values nil expressions1))))))


(defmacro ano-loop-parser (name inner-parser-name &body validate-and-return-form)
  (let ((name (get-parser-name name))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))
        
    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

       (when *debug-p*
         (terpri)
         (pprint (list :ano-loop-call ',name expressions)))

       (if (consp expressions)

           (let ((done nil)
                 (response nil)
                 (responses nil))
             
             (declare (ignorable done))

             (loop while ;(and expressions (not done)) do
                   expressions do

                   (setf done t)

                   (when *debug-p*
                     (terpri)
                     (pprint (list :in-ano-loop ',name expressions responses)))

                   (multiple-value-setq (response expressions)

                       (apply (symbol-function ',(get-parser-name inner-parser-name))
                              expressions args))

                   (unless response (setf expressions (cdr expressions)))

                   (when ;response
                       t
                     (setf done nil)
                     (push response responses)))

             (setf response (nreverse responses))

             (multiple-value-bind (response expressions)
                 (progn 
                   ,@validate-and-return-form)

               (when *debug-p*
                 (terpri)
                 (pprint (list :ano-loop-return ',name response expressions)))
                 
               (values response expressions)))

         (values nil expressions))))))


(defmacro ano-optional-loop-parser (name inner-parser-name &body validate-and-return-form)
  (let ((name (get-parser-name name))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))
        
    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (when *debug-p*
           (terpri)
           (pprint (list :ano-loop-call ',name expressions)))

         (if (consp expressions)

             (let ((done nil)
                   (response nil)
                   (responses nil))
             
               (declare (ignorable done))

               (loop while ;(and expressions (not done)) do
                     expressions do

                     (setf done t)

                     (when *debug-p*
                       (terpri)
                       (pprint (list :in-ano-optional-loop ',name expressions responses)))

                     (let ((orig expressions))

                       (multiple-value-setq (response expressions)

                           (apply (symbol-function ',(get-parser-name inner-parser-name))
                                  expressions args))

                       (cond ((not response)
                              (setf expressions nil)

                              (multiple-value-bind (response expressions)
                                  (progn 
                                    ,@validate-and-return-form)

                                (declare (ignorable expressions))

                                (when *debug-p*
                                  (terpri)
                                  (pprint (list :ano-optional-loop-return ',name response orig)))

                                (setf response (nreverse responses))
                            
                                (return-from ,name
                                  (values responses orig))))                          

                             (t
                              (setf done nil)
                              (push response responses)))))

               (setf response (nreverse responses))

               (multiple-value-bind (response expressions)
                   (progn 
                     ,@validate-and-return-form)

                 (when *debug-p*
                   (terpri)
                   (pprint (list :loop-return ',name response expressions)))
                 
                 (values response expressions)))

           (values nil expressions))))))

(defmacro choice-parser (type attributes-and-binders choice-parsers &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))

    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (if (consp expressions)

             (progn 
              
               (when *debug-p*
                 (terpri) 
                 (pprint (list :choice-call ',name expressions)))

               (dolist (parser ',choice-parsers)

                 (when *debug-p*
                   (terpri)
                   (pprint (list :in-choice-loop *kb* ',name expressions)))

                 (multiple-value-bind (attributes expressions1)
                     (apply #'get-attributes (first expressions)
                            ',(first attributes-and-binders) args)

                   (declare (ignorable attributes))

                   (let ((expressions (cons expressions1 (cdr expressions))))

                     (,@(second attributes-and-binders)

                      (multiple-value-bind (response expressions)
                          (apply (symbol-function (get-parser-name parser) )
                                 expressions
                                 args)

                        (when response
                        
                          (multiple-value-bind (response expressions)
                              (progn 
                                ,@validate-and-return-form)
                          
                            (when *debug-p*
                              (terpri)
                              (pprint (list :choice-return ',name response expressions)))
                          
                            (return-from ,name
                              (values response expressions)))))))))

               (when error-p
                 (return-from parser 
                   (values
                    (owllink-syntax-error-message :error 
                                                  (error-message
                                                   "No valid OWLlink ~A request: ~S" 
                                                   ',type
                                                   (expand-functional-tag-name
                                                    (first (first expressions)))))
                    (rest expressions))))

               (values nil (rest expressions)))

           (values nil expressions))))))


(defmacro choice-parser-with-timeout (type attributes-and-binders choice-parsers &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))

    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (if (consp expressions)

             (progn 
              
               (when *debug-p*
                 (terpri) 
                 (pprint (list :choice-call ',name expressions)))

               (dolist (parser ',choice-parsers)

                 (when *debug-p*
                   (terpri)
                   (pprint (list :in-choice-loop *kb* ',name expressions)))

                 (multiple-value-bind (attributes expressions1)
                     (apply #'get-attributes (first expressions)
                            ',(first attributes-and-binders) args)

                   (declare (ignorable attributes))

                   (let ((expressions (cons expressions1 (cdr expressions))))

                     (,@(second attributes-and-binders)

                      (multiple-value-bind (response expressions)
                          #+:racer-server
                        (racer::with-timeout (*server-timeout* 
                                              (values
                                               (owllink-error-message 
                                                :error 
                                                (error-message
                                                 "Timeout after ~A seconds for request: ~S" 
                                                 *server-timeout*
                                                 (expand-functional-tag-name
                                                  (first (first expressions)))))
                                               (rest expressions)))
                          (apply (symbol-function (get-parser-name parser) )
                                 expressions
                                 args))
                        #-:racer-server
                        (apply (symbol-function (get-parser-name parser) )
                               expressions
                               args)

                        (when response
                        
                          (multiple-value-bind (response expressions)
                              (progn 
                                ,@validate-and-return-form)
                          
                            (when *debug-p*
                              (terpri)
                              (pprint (list :choice-return ',name response expressions)))
                          
                            (return-from ,name
                              (values response expressions)))))))))

               (when error-p
                 (return-from parser 
                   (values
                    (owllink-syntax-error-message :error 
                                                  (error-message
                                                   "No valid OWLlink ~A request: ~S" 
                                                   ',type
                                                   (expand-functional-tag-name
                                                    (first (first expressions)))))
                    (rest expressions))))

               (values nil (rest expressions)))

           (values nil expressions))))))



(defmacro optional-choice-parser (type attributes-and-binders choice-parsers &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))

    `(defun ,name (expressions &rest args)
       (block parser

       (if (consp expressions)

           (progn 
             
             (when *debug-p*
               (terpri) 
               (pprint (list :choice-call ',name expressions)))

             (dolist (parser ',choice-parsers)

               (when *debug-p*
                 (terpri)
                 (pprint (list :in-choice-loop *kb* ',name expressions)))

               (multiple-value-bind (attributes expressions1)
                   (apply #'get-attributes (first expressions)
                          ',(first attributes-and-binders) args)

                 (declare (ignorable attributes))

                 (let ((expressions (cons expressions1 (cdr expressions))))

                   (,@(second attributes-and-binders)

                    (multiple-value-bind (response expressions)
                        (apply (symbol-function (get-parser-name parser) )
                               expressions
                               args)

                      (when response
                        
                        (multiple-value-bind (response expressions)
                            (progn 
                              ,@validate-and-return-form)
                          
                          (when *debug-p*
                            (terpri)
                            (pprint (list :choice-return ',name response expressions)))
                          
                          (return-from ,name
                            (values response expressions)))))))))

             (values
              nil
              expressions))

         (values nil expressions))))))


(defmacro tag-parser (type attributes-and-binders &body validate-and-return-form)

    (let ((name (get-parser-name type))
          (attributes-and-binders
           (or attributes-and-binders
               (list nil '(progn))))
          (validate-and-return-form
           (or validate-and-return-form '((values response)))))

      `(defun ,name (expressions1 &rest args &key (error-p t))
         (declare (ignorable error-p))

         (block parser
  
           (when *debug-p*
           (terpri) 
           (pprint (list :tag-call ',name expressions1)))

         (if (and (consp (first expressions1))
                  (eq (expand-functional-tag-name 
                       (first (first expressions1)))
                      ',type))

             (multiple-value-bind (attributes expressions)
                 (apply #'get-attributes (first expressions1) ',(first attributes-and-binders) args)
               (declare (ignorable attributes expressions))
               
               (setf expressions (cddr expressions)) ; remove tag and attributes

               (,@(second attributes-and-binders)

                (multiple-value-bind (response)
                    (progn 
                      ,@validate-and-return-form)

                  (when *debug-p*
                    (terpri)
                    (pprint (list :tag-return ',name response (rest expressions1))))

                  (values response (rest expressions1)))))

           (values nil expressions1))))))

(defmacro dummy-tag-parser (name &body body)

  (let ((name (get-parser-name name)))

    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p args))

       (block parser
         
         (when *debug-p*
           (terpri) 
           (pprint (list :dummy-tag-parser-call ',name expressions)))

         ,@body))))


(defmacro with-parser-call ((parser expressions &rest args) &body body)
  `(progn
    
     (block parser

       (multiple-value-bind (response expressions)
           (apply (symbol-function (get-parser-name ',parser))
                  ,expressions
                  ,args)

         (declare (ignorable response expressions))

         ,@body))))

