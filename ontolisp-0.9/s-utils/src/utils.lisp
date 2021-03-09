;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is a collection of Common Lisp utilities
;;;;
;;;; Copyright (C) 2004-2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-utils)

(export
 '(make-subdirectory
   pathname-parent
   copy-stream
   tokens
   format-universal-time
   format-duration
   format-iso-gmt-time
   parse-integer-safely
   +us-day-names+
   +us-month-names+
   +us-time-format+
   +us-date-format+
   +en-duration-unit-names+))

;; pathname/directory manipulation

(defun make-subdirectory (basedir subdir)
  "Give a pathname, basedir, of a directory, create a subdirectory with name subdir"
  (make-pathname :directory (append (pathname-directory basedir)
				    (if (listp subdir) subdir (list subdir)))))

(defun pathname-parent (pathname)
  "Given a pathname, return the parent pathname"
  (if (pathname-name pathname)
      (make-pathname :directory (pathname-directory pathname))
    (make-pathname :directory (butlast (pathname-directory pathname)))))

;; stream copying

(defun copy-stream (in out &optional (buffer (make-string 4096)))
  "Copy all data from input stream in to output stream out using buffer (and read/write-sequence)"
  (labels ((copy-chunks ()
             (let ((size (read-sequence buffer in)))
               (if (< size (length buffer))
                   (write-sequence buffer out :end size)
                 (progn
                   (write-sequence buffer out)
                   (copy-chunks))))))
    (copy-chunks)))

;; elementary parsing

(defun tokens (string &key (start 0) end (separators (list #\space #\return #\linefeed #\tab)))
  "Split string in a list of tokens using separators, a list of characters"
  (if (= start (length string))
      '()
    (let ((p (position-if #'(lambda (char) (find char separators :test #'char=)) 
                          string 
                          :start start :end end)))
      (if p
	  (if (= p start)
	      (tokens string :start (1+ start) :end end :separators separators)
	    (cons (subseq string start p)
		  (tokens string :start (1+ p) :end end :separators separators)))
	(list (subseq string start end))))))

;; time/date formatting

(defparameter +us-day-names+ 
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "US English short day name constant strings")

(defparameter +us-month-names+ 
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "US English short month name constant strings")

(defparameter +us-time-format+ 
  '(:day-name #\Space :month-name #\Space :date #\Space :year #\Space :hour #\: :minute #\: :second)
  "US English style date-time format")

(defparameter +us-date-format+ 
  '(:day-name #\Space :month-name #\Space :date #\Space :year)
  "US English style date-only format")

(defun format-universal-time (universal-time 
                              &key 
                              (format +us-time-format+) 
                              (day-names +us-day-names+)  
                              (month-names +us-month-names+)
                              decode-in-timezone
                              stream)
  "Format universal time using format, day-names and month-names, if stream is not nil use it, else return a string"
  (multiple-value-bind (second minute hour date month year day daylight-p timezone)
      (if decode-in-timezone 
          (decode-universal-time universal-time decode-in-timezone)
        (decode-universal-time universal-time))
    (declare (ignore daylight-p))
    (flet ((two-digit (n) (format nil "~2,'0d" n)))
      (let* ((month-name (elt month-names (1- month)))
             (day-name (elt day-names day))
             (bindings `((:second . ,(two-digit second)) 
                         (:minute . ,(two-digit minute)) 
                         (:hour . ,(two-digit hour)) 
                         (:date . ,date) 
                         (:month . ,month) 
                         (:year . ,year) 
                         (:day . ,day)
                         (:date2 . ,(two-digit date))
                         (:day-name . ,day-name) 
                         (:month-name . ,month-name)
                         (:timezone . ,(format nil "~c~2,'0d" (if (plusp timezone) #\+ #\-) (abs timezone)))))
             (out (or stream (make-string-output-stream))))
        (dolist (x format)
          (format out "~a" (if (keywordp x) (cdr (assoc x bindings)) x)))
        (unless stream
          (get-output-stream-string out))))))

;; duration formatting

(defparameter +en-duration-unit-names+
  #("year" "day" "hour" "minute" "second")
  "English time duration unit name constant strings")

(defun format-duration (seconds &key (unit-names +en-duration-unit-names+) stream)
  "Format seconds as duration using unit-names, if stream is not nil use it, else return a string"
  (let ((out (or stream (make-string-output-stream)))
        years days hours minutes did-wrote-output)
    (setf years (floor seconds (* 60 60 24 365)))
    (setf seconds (rem seconds (* 60 60 24 365)))
    (setf days (floor seconds (* 60 60 24)))
    (setf seconds (rem seconds (* 60 60 24)))
    (setf hours (floor seconds (* 60 60)))
    (setf seconds (rem seconds (* 60 60)))
    (setf minutes (floor seconds 60))
    (setf seconds (rem seconds 60))
    (flet ((fmt-unit (n unit)
             (unless (zerop n)
               (when did-wrote-output (write-char #\space out))
               (format out "~d ~a~p" n unit n)
               (setf did-wrote-output t))))
      (fmt-unit years (aref unit-names 0))
      (fmt-unit days (aref unit-names 1))
      (fmt-unit hours (aref unit-names 2))
      (fmt-unit minutes (aref unit-names 3))
      (fmt-unit seconds (aref unit-names 4)))
    (unless stream
      (get-output-stream-string out))))

;; simplified ISO date/time formatting

(defun format-iso-gmt-time (universal-time &key stream)
  "Format universal time using a simple and fast 'ISO GMT' style, if stream is not nil use it, else return a string"
  (let ((out (or stream (make-string-output-stream))))
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time universal-time 0)
      (flet ((two-digit (n s) (if (< n 10) 
                                  (progn (write-char #\0 s) (write n :stream s)) 
                                (write n :stream s))))
        (write year :stream out)
        (two-digit month out)
        (two-digit date out)
        (write-char #\T out)
        (two-digit hour out)
        (two-digit minute out)
        (two-digit second out)))
    (unless stream
      (get-output-stream-string out))))
    
;; extended integer parsing

(defun parse-integer-safely (string &key (start 0) end (radix 10) default)
  "Like parse-integer, but will return default on error, accepts nil as argument"
  (if (and (stringp string) 
           (not (zerop (- (length string) start))))
      (multiple-value-bind (value terminating-position)
          (parse-integer string :start start :end end :radix radix :junk-allowed t)
        (if (= terminating-position (or end (length string)))
            value
          default))
    default))

;;;; eof
