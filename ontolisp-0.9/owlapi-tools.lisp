;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

(in-package :owlapi)

;;;
;;;;  owlapi-tools.lisp
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
;;;   Purpose: Some auxiliary functions for the OWLAPI 
;;; 

(defvar *sym-count* 0)

(defconstant +secret-prefix+ 'secret-cdsc7897qcjk)

(defconstant +default-hash-table-size+ 100)

(defun set-equal (a b &rest args)
  (and (apply #'subsetp a b args)
       (apply #'subsetp b a args)))

#-:racer-server
(defun to-keyword (x)
  (intern (format nil "~A" x) :keyword))

(defun ensure-list (arg)
  (if (listp arg)
      arg
    (list arg)))

(defun ensure-string (string) 
  (if (stringp string)
      string
    (if (symbolp string)
        (symbol-name string)
      (format nil "~A" string))))

;;;
;;;
;;;

(defun ends-with-#-p (prefix)
  (let ((string
         (ensure-string prefix)))
    (unless (zerop (length string)) 
      (char= #\# 
             (elt string 
                  (1- (length string)))))))
  
(defun starts-with-#-p (prefix)
  (let ((string
         (ensure-string prefix)))
    (unless (zerop (length string)) 
      (char= #\# 
             (elt string 
                  0)))))

(defun without-# (string)
  (if (stringp string)
      (let ((n (length string)))
        (if (char= #\# (elt string (1- n)))
            (subseq string 0 (1- n))
          string))
    string))

(defun without-starting-# (string)
  (if (stringp string)
      (if (char= #\# (elt string 0))
          (subseq string 1)
        string)
    string))

(defun with-# (string)
  (format nil "~A#" (without-# string)))

;;;
;;;
;;; 


(defun ends-with-colon-p (prefix)
  (let ((string
         (ensure-string prefix)))
    (unless (zerop (length string)) 
      (char= #\: 
             (elt string 
                  (1- (length string)))))))
  
(defun ensure-ends-with-colon (prefix)
  (if (ends-with-colon-p prefix)
      prefix
    (let ((res 
           (concatenate 'string (ensure-string prefix) ":")))
      (etypecase prefix
        (keyword (to-keyword res))
        (symbol (intern res))
        (string res)))))


(defun without-colon (prefix)
  (let ((prefix (ensure-string prefix)))
    (if (ends-with-colon-p prefix)
        (subseq prefix 0 (1- (length prefix)))
      prefix)))

(defun get-prefix-postfix (symbol) 
  (let* ((name (ensure-string symbol))
         (pos (position #\: name)))
    (if pos
        (values (intern (subseq name 0 (1+ pos)))
                (intern (subseq name (1+ pos))))
      (values nil symbol))))

;;;
;;;
;;;

#+:racer
(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (racer::racer-make-hash-table :size size :rehash-size 2.0 :test test))

#-:racer
(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (make-hash-table :size size :rehash-size 2.0 :test test))

(defun create-marker (&optional sym (new-p t))
  (if sym       
      (if new-p 
          (intern (format nil "~A-~A-~A" +secret-prefix+ sym (incf *sym-count*)) :cl-user)
        (intern (format nil "~A-~A" +secret-prefix+ sym)  :cl-user))
    (intern (format nil "~A-~A" +secret-prefix+ (incf *sym-count*))  :cl-user)))


#+:racer-server
(defun owlapi-warning (string &rest args)
  (apply #'ts::nrql-warning
         (concatenate 'string "~%~%OWLAPI Warning: " 
		      (ensure-string string))
         args))

#-:racer-server
(defun owlapi-warning (string &rest args)
  (apply #'format
         t
         (concatenate 'string "~%~%OWLAPI Warning: " 
		      (ensure-string string))
         args))

(defun owlapi-runtime-error (string &rest args)
  (apply #+:racer-server
         #'ts::nrql-error 
         #-:racer-server
         #'error
         (concatenate 'string "OWLAPI Runtime Error: " 
		      (ensure-string string))
         args))

(defun owlapi-parser-error (string &rest args)
  (apply #+:racer-server
         #'ts::nrql-error
         #-:racer-server
         #'error
         (concatenate 'string "OWLAPI Parser Error: " 
		      (ensure-string string))
         args))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-transform (string)
    (ecase (readtable-case *readtable*)
      (:upcase (if (char= (aref string 0) #\|)
                 string
                 (string-upcase string)))
      (:preserve string)
      (:downcase (string-downcase string)))))

;;;
;;;
;;;

#+(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list 
	       `(persistence-manager:defclass ,@rest)
               `(defun ,(intern (format nil "~A-~A-~A"
                                        (string-transform "is") name (string-transform "p")))
                       (obj)
                  (typep obj ',name))))))

#-(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentclass (&rest rest)
  `(defclass ,@rest))

;;;
;;;
;;;

#+(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentstruct (name &rest args)
  `(persistence-manager::defstruct ,name ,@args))
  
#-(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentstruct (name &rest args)
  (let ((name
         (if (consp name)
             (remove-if (lambda (x)
                          (and (consp x)
                               (eq (first x) :package)))
                        name)
           name)))
  `(defstruct ,name ,@args)))


;;;
;;;
;;;

(defmacro owlapi-defun ((name &key doc dont-export)
                        lambda-list 
                        &body body)
  #-:racer-server
  (declare (ignorable doc dont-export))
  #+:racer-server
  `(nrql-defun (,name :doc ,doc :dont-export ,dont-export) ,lambda-list ,@body)
  #-:racer-server
  `(defun ,name ,lambda-list ,@body))

(defmacro owlapi-defmethod ((name &key doc dont-export)
                        lambda-list 
                        &body body)
  #-:racer-server
  (declare (ignorable doc dont-export))
  #+:racer-server
  `(nrql-defmethod (,name :doc ,doc :dont-export ,dont-export) ,lambda-list ,@body)
  #-:racer-server
  `(defmethod ,name ,lambda-list ,@body))


(defmacro => (a b)
  `(or (not ,a) ,b))

(defun is-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (or 
     (and (> (length name) 6)
          (or (string-equal "http://" 
                            (subseq name 0 7))
              (string-equal "file://" 
                            (subseq name 0 7))))
     (and (> (length name) 7)
          (or (string-equal "https://" 
                            (subseq name 0 8)))))))
     
(defun is-file-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (and (is-url-p x)
         (> (length name) 6)
         (string-equal "file://" 
                       (subseq name 0 7)))))

(defun is-http-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (and (is-url-p x)
         (or (and (> (length name) 6)
                  (string-equal "http://" 
                                (subseq name 0 7)))
             (and (> (length name) 7)
                  (string-equal "https://" 
                                (subseq name 0 8)))))))


(defparameter *replacements*
  `(("&" "&amp;" nil)
    ("!empty" "&nbsp;" nil)
    ("ä" "&auml;" nil)
    ("ö" "&ouml;" nil)
    ("ü" "&uuml;" nil)
    ("Ä" "&Auml;" nil)
    ("Ö" "&Ouml;" nil)
    ("Ü" "&Uuml;" nil)
    ("ß" "&szlig;" nil)
    ("<" "&lt;" nil)
    (">" "&gt;" nil)
    ("\\@" "&#64;" nil)))


#-:racer-server
(defun whitespace-char-p (char)
  #+:lispworks
  (lispworks:whitespace-char-p char)
  #+:allegro
  (stream::whitespace-char-p char)
  #+:sbcl
  (sb-impl:whitespace[2]p char)
  #+(and (not :allegro) (not :lispworks) (not :sbcl))
  (to-be-implemented 'whitespace-char-p))

#-:racer-server
(defun blank-line-p (line)
  (or (eq line 'newline)
      (and (typep line 'string)
	   (not (position-if-not #'(lambda (i) (char= i #\space)) line)))))


#-:racer-server
(defun string-substitute (string &optional (rules *replacements*)
			         &key add-spaces)
  (labels ((do-it (string akku)
             (cond ((blank-line-p string) akku)
                   (t 
                    (let ((min-pos nil)
                          (min-from-to))
                      (dolist (from-to rules)
                        (let* ((from (first from-to))
                               (pos (search from string)))
                          (when pos
                            (if (or (not min-pos) 
                                    (< pos min-pos))
                                (setf min-from-to from-to
                                      min-pos pos)))))
                      (let ((from (first min-from-to))
                            (to (second min-from-to))
                            (replaced-as-new-input-p (third min-from-to)))
                        (if min-pos
                            (if replaced-as-new-input-p
                                (do-it 
                                 (concatenate 'string 
					      to
					      (subseq string (+ min-pos (length from))))
                                 (append akku 
                                         (list (subseq string 0 min-pos))))
                              (do-it 
                               (subseq string (+ min-pos (length from)))
                               (append akku 
                                       (list (subseq string 0 min-pos))
                                       (list to))))
                          (append akku (list string)))))))))
      
    (let ((res (do-it (if add-spaces 
                          (concatenate 'string " " string " ")
                        string)
		      nil)))
      (if res 	  
          (reduce #'(lambda (x y)
                      (concatenate 'string x y))
                  res)
        ""))))




(defmacro defmethod1 (name lambda-list &body body)
  `(owlapi-defmethod (,name :dont-export t)
     ,lambda-list
     ,@body))

(defmacro defun1 (name lambda-list &body body)
  `(owlapi-defun (,name :dont-export t)
     ,lambda-list
     ,@body))


(defun string-to-boolean (x)
  (if (member x '(nil t))
      x
    (when (stringp x)
      (cond ((string= x "true") t)
            ((string= x "false") nil)
            ((string= x "t") t)
            ((string= x "T") t)
            ((string= x "nil") nil)
            ((string= x "NIL") nil)
            (t nil)))))


#+:racer-server
(defun to-be-implemented (method)
  (nrql-error "~A: To be implemented" method))

#-:racer-server
(defun to-be-implemented (method)
  (error "~A: To be implemented" method))

;;;
;;;
;;;

#-:racer-server
(defun shrink-whitespaces (string)
  (if (stringp string)
      (if (every #'whitespace-char-p string)
          ""
        (let ((pos (or (position-if-not #'whitespace-char-p string) 0))
              (pos2 (or (position-if-not #'whitespace-char-p string :from-end t)
                        (1- (length string)))))
          (subseq string pos (1+ pos2))))
    string))

(defun clean-url (url)
  (if url 
      (let ((pos (position #\space url)))
	(if pos
	    (concatenate 'string
	      (subseq url 0 pos)
	      "%20"
	      (clean-url (subseq url (1+ pos))))
	  url))
    ""))


(defun decode-month (m)
  (case m 
    (1 "January")
    (2 "February")
    (3 "March")
    (4 "April")
    (5 "May")
    (6 "June")
    (7 "July")
    (8 "August")
    (9 "September")
    (10 "October")
    (11 "November")
    (12 "December")))


(defun get-current-date ()
  #+:racer-server
  (licgen::decode-date1 (get-universal-time) t)
  #-:racer-server
  (multiple-value-bind (s mi h d mo year)
      (decode-universal-time (get-universal-time))
    (declare (ignorable s))
    (format nil "~2,'0D ~2,'0D ~2,'0D, ~2,'0D:~2,'0D" (decode-month mo) d year 
            h mi)))

;;;
;;;
;;;

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


