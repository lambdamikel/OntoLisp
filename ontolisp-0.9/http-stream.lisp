;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

(in-package :owl-syntaxes)

;;;
;;;;  http-stream.lisp
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
;;;   Purpose: Opens and provides an HTTP stream for a given URL from which we can read.
;;;            

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


(defun without-file-prefix (url)
  (let ((res (search "file://" url)))
    (if (and res
             (zerop res))
        (subseq url 7)
      url)))

#+:aserve
(defmacro with-input-from-url ((stream url &key close-manually-p) &body body)
  (let ((orig (gensym)))

    `(let* ((,orig (if (symbolp ,url)
		       (symbol-name ,url)
		     ,url))
	    (,url 
	     #+:racer-server
	     (racer:check-for-url-mirror ,orig)
	     #-:racer-server
	     ,orig))

       #+:racer-server
       (unless (equalp ,orig ,url)
         (when racer::*tbox-verbose*
           (format t "~V@TURL ~A mirrored to ~A." racer::*indent* ,orig ,url)))                    
     
       ,(if close-manually-p
	    `(if (owlapi:is-file-url-p ,url)
		 
		 (let ((,stream 
			(open (without-file-prefix ,url)
			      :direction :input)))
		   ,@body)
	       
	       (break "not yet supported"))

	  `(if (owlapi:is-file-url-p ,url)
	
	       (with-open-file (,stream (without-file-prefix ,url))
		 ,@body)
    
	     (multiple-value-bind (body res headers)
		 (let ((,url 
			(clean-url ,url)))
		   (net.aserve.client:do-http-request ,url
		     #+:racer-server
		     :proxy 
		     #+:racer-server
		     racer:*proxy*
		     :headers '(("Accept-Encoding" . "gzip"))
		     :format :binary
		     ;; :external-format :latin1-base
		     ))
		    
	       (cond ((equal (cdr (assoc :content-encoding headers)) "gzip")

		      #+:racer-server
		      (when *tbox-verbose*
			(format t "HTTP request returned ~s gzip'ed bytes, " (length body)))
			   
		      (let ((so (make-string-output-stream))
			    (bis (excl:make-buffer-input-stream body))
			    (bi))
			      
			(util.zip::skip-gzip-header bis)

			(setq bi (make-instance 'util.zip:inflate-stream :input-handle bis))
			     
			(do ((byte (read-byte bi nil nil) (read-byte bi nil nil)))
			    ((null byte)
			     (setq body (get-output-stream-string so))
				  
			     #+:racer-server
			     (when *tbox-verbose*
			       (format t "~s characters after inflation~%" (length body))))
			       
			  (write-char (code-char byte) so))))
			   
		     (t (setq body (excl:octets-to-string body))))
		    
	       (cond ((not (eql 200 res))
		      (error "HTTP request returned code ~s" res))
			  
		     (t 
		      (with-input-from-string (,stream body)
			,@body)))))))))


#+:cl-http
(defmacro with-input-from-url ((stream url &key close-manually-p) &body body)
  `(let* ((url-spec (if (symbolp ,url)
                        (symbol-name ,url)
                      ,url))
          (real-url-spec (racer:check-for-url-mirror url-spec))
          (url (url:intern-url real-url-spec))
          (headers nil))

     (declare (ignorable headers url))

     (unless (equalp url-spec real-url-spec)
       (when racer::*tbox-verbose*
         (format t "~V@TURL ~A mirrored to ~A." racer::*indent* url-spec real-url-spec)))
     
     ,(if close-manually-p
          `(if (owlapi:is-file-url-p real-url-spec)
	
               (let ((,stream 
                      (open (without-file-prefix real-url-spec)
                            :direction :input)))
                 ,@body)

             (break "not yet supported"))

        `(if (owlapi:is-file-url-p real-url-spec)
	
             (with-open-file (,stream (without-file-prefix real-url-spec))
               ,@body)

           (progn 
             (when *tbox-verbose*
               (unless (string= url-spec real-url-spec)
                 (format t "URL ~A mirrored to ~A." url-spec real-url-spec)))

             (racer:with-temp-file ((,stream)
                                     (http:show-url url :stream stream :headers headers))
               ,@body))))))


#+(and (not :aserve) (not :cl-http))
(defmacro with-input-from-url ((stream url &key close-manually-p) &body body)
  (declare (ignorable close-manually-p))
  `(if (owlapi:is-file-url-p ,url)
       (let ((,stream 
              (open (without-file-prefix ,url)
                    :direction :input)))
         ,@body)
     (let ((string 
            (s-http-client:do-http-request ,url)))
       ;; (pprint string)
       (with-input-from-string (,stream string)
         ,@body))))
