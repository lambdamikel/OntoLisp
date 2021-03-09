;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; Bivalent streams for CLISP
;;;;
;;;; Copyright (C) 2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-sysdeps)

;;; Rationale:

;;; we want (socket) streams that support input and output (i.e. are bidirectional), 
;;; using bytes and characters (i.e. are bivalent) at the same time, with conversions
;;; using a straight/native byte to character conversion

;;; this is an (incomplete) implementation for CLISP based on Gray streams
;;; in other words: this is a hack to fix a particular problem/situation

(defclass bivalent-bidirectional-stream (gray:fundamental-character-input-stream
                                         gray:fundamental-character-output-stream
                                         gray:fundamental-binary-input-stream
                                         gray:fundamental-binary-output-stream)
  ((bidirectional-binary-stream :reader get-native-stream :initarg :bidirection-binary-stream)))

(defun make-bivalent-stream (stream)
  "Wrap a bidirectional binary stream so that it behaves as a bivalent bidirectional stream"
  (make-instance 'bivalent-bidirectional-stream :bidirection-binary-stream stream))

;;; minimal required methods

(defmethod stream-element-type ((stream bivalent-bidirectional-stream))
  '(or character (unsigned-byte 8)))

(defmethod close ((stream bivalent-bidirectional-stream) &key abort)
  (close (get-native-stream stream) :abort abort)
  (call-next-method))

(defmethod gray:stream-position ((stream bivalent-bidirectional-stream) position)
  (gray:stream-position (get-native-stream stream) position))

(defmethod gray:stream-read-char ((stream bivalent-bidirectional-stream))
  (code-char (read-byte (get-native-stream stream))))

#+nil
(defmethod gray:stream-unread-char ((stream bivalent-bidirectional-stream)))

(defmethod gray:stream-write-char ((stream bivalent-bidirectional-stream) char)
  (write-byte (char-code char) (get-native-stream stream)))

(defmethod gray:stream-line-column ((stream bivalent-bidirectional-stream))
  nil)

(defmethod gray:stream-finish-output ((stream bivalent-bidirectional-stream))
  (finish-output (get-native-stream stream)))

(defmethod gray:stream-force-output ((stream bivalent-bidirectional-stream))
  (force-output (get-native-stream stream)))

(defmethod gray:stream-read-byte ((stream bivalent-bidirectional-stream))
  (read-byte (get-native-stream stream)))

(defmethod gray:stream-read-byte-lookahead ((stream bivalent-bidirectional-stream))
  (ext:read-byte-lookahead (get-native-stream stream)))

(defmethod gray:stream-write-byte ((stream bivalent-bidirectional-stream) byte)
  (write-byte byte (get-native-stream stream)))

;;; 'optimized' sequence IO

(defmethod gray:stream-read-char-sequence ((stream bivalent-bidirectional-stream) sequence 
                                           &optional start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (let* ((byte-buffer (make-array (- end start) :element-type '(unsigned-byte 8)))
         (result (ext:read-byte-sequence byte-buffer (get-native-stream stream))))
    (loop :for i :from start :below (min end result) 
          :do (setf (elt sequence (+ start i)) (code-char (elt byte-buffer i))))
    (+ start result)))

(defmethod gray:stream-write-char-sequence ((stream bivalent-bidirectional-stream) sequence 
                                            &optional start end)
  (unless start (setf start 0))
  (unless end (setf end (length sequence)))
  (let ((byte-buffer (make-array (- end start) :element-type '(unsigned-byte 8))))
    (loop :for i :from start :below (- end start) 
          :do (setf (elt byte-buffer i) (char-code (elt sequence (+ start i)))))
    (multiple-value-bind (seq result)
        (ext:write-byte-sequence byte-buffer (get-native-stream stream))
      (declare (ignore seq))
      (+ start result))))

(defmethod gray:stream-read-byte-sequence ((stream bivalent-bidirectional-stream) sequence 
                                           &optional start end no-hang interactive)
  (declare (ignore no-hang interactive))
  (ext:read-byte-sequence sequence (get-native-stream stream) :start start :end end))

(defmethod gray:stream-write-byte-sequence ((stream bivalent-bidirectional-stream) sequence 
                                            &optional start end no-hang interactive)
  (declare (ignore no-hang interactive))
  (ext:write-byte-sequence sequence (get-native-stream stream) :start start :end end))

;;;; eof
