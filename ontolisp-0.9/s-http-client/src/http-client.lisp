;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; This is the implementation of S-HTTP-CLIENT
;;;;
;;;; Copyright (C) 2005,2006,2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-client)

;; data structures for state management

(defclass http-client-state () 
  ((data :initform nil)
   (lock :reader get-lock :initform (s-sysdeps:make-process-lock "http-client-state")))
  (:documentation "Object holding all HTTP client state"))

(defun make-http-client-state ()
  "Make a new HTTP client state object to hold open (keepalive) connections"
  (make-instance 'http-client-state))

(defvar *default-http-client-state* (make-http-client-state)
  "The default, globally shared HTTP client state")

(defclass http-server-state ()
  ((scheme-host-port :accessor get-scheme-host-port :initarg :scheme-host-port)
   (socket :accessor get-socket :initarg :socket :initform nil)
   (in-use-p :accessor get-in-use-p :initform nil)
   (buffer :accessor get-buffer :initform (make-string 4096))
   (timestamp :accessor get-timestamp :initform (get-universal-time)))
  (:documentation "Object holding a reusable open connection to a scheme://host:port"))

(defmethod print-object ((state http-server-state) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "~a" (get-scheme-host-port state))))

(defmethod get-kept-alive-age ((http-server-state http-server-state))
  (- (get-universal-time) (get-timestamp http-server-state)))

;; low level output

(defun write-http-request-line (string &optional (stream *standard-output*))
  (write-string string stream)
  (write-char #\return stream)
  (write-char #\linefeed stream))

(defun format-http-request-line (stream format-string &rest args)
  (write-http-request-line (apply #'format nil format-string args) stream))

;; some HTTP protocol defaults

(defvar *http-client-agent* 
  (format nil "HTTP Client ~a ~a" (lisp-implementation-type) (lisp-implementation-version))
  "The value to use for the 'User-Agent' request header")

(defvar *http-client-accept* 
  "*/*"
  "The value to use for the 'Accept' request header")

;; low level input - we are using a reuable buffer to read lines

(defun read-crlf-line (buffer stream &optional (eof-error-p t) eof-value)
  "Read a CRLF termintated line from a character input stream into buffer. Return length excluding CRLF."
  (let ((offset 0)
        (previous-char #\null))
    (loop :for char = (read-char stream eof-error-p eof-value)
          :do (cond ((equal char eof-value) 
                     (return eof-value))
                    ((and (char= char #\linefeed)
                          (char= previous-char #\return))
                     (return (1- offset)))
                    ((>= offset (length buffer))
                     (error "Line length exceeds buffer size (~d)" (length buffer)))
                    (t 
                     (setf (char buffer offset) char)
                     (setf previous-char char)
                     (incf offset))))))

;; low level HTTP input

(defun response-read-code (stream buffer)
  (let* ((line-length (read-crlf-line buffer stream))
         (first-space (position #\space buffer :test #'char= :end line-length)))
    (parse-integer buffer :start (1+ first-space) :end line-length :junk-allowed t)))

(defparameter +common-response-headers+
  (mapcar #'(lambda (header) (cons header (intern (string-upcase header) :keyword)))
          '("Server" "Date" 
            "Content-Length" "Content-Type"
            "Connection" "Keep-Alive" "Cache-Control" "ETag" "Last-Modified")))

(defun header-field-name->keyword (string &optional (start 0) end)
  ;; optimize the case of common response headers and avoid interning/upcasing
  (let ((common-header (find-if #'(lambda (x) 
                                    (string-equal (car x) string :start2 start :end2 end))
                                +common-response-headers+)))
    (if common-header
        (cdr common-header)
      (intern (nstring-upcase (subseq string start end)) :keyword))))

(defun header-field-value->string (string &optional (start 0) end)
  ;; skip leading whitespace
  (loop :while (and (< start end) 
                    (member (char string start) '(#\space #\tab) :test #'char=))
        :do (incf start))
  (subseq string start end))

(defun response-read-headers (stream buffer)
  (loop :for line-length = (read-crlf-line buffer stream nil)
        :until (or (null line-length)
                   (zerop line-length))
        :collect (let ((colon (position #\: buffer :end line-length :test #'char=)))
                   (cons (header-field-name->keyword buffer 0 colon)
                         (header-field-value->string buffer (1+ colon) line-length)))))

(defun concatenate-chunks (chunks total-size)
  ;; (apply #'concatenate 'string chunks) does the same, is our code better ?
  (let ((buffer (make-string total-size))
        (offset 0))
    (loop :for chunk :in chunks :do
          (replace buffer chunk :start1 offset)
          (incf offset (length chunk))) 
    buffer))

(defun response-read-chunked-body (stream body buffer)
  ;; content-length is unknown/deferred, each chunk size is known
  (let ((total-size 0)
        chunks
        line-length
        chunk-size)
    (loop
     (setf line-length (read-crlf-line buffer stream nil))
     (setf chunk-size (parse-integer buffer :end line-length :radix 16 :junk-allowed t))
     (incf total-size chunk-size)
     (if (zerop chunk-size)
         (return)
       (let ((total-chunk-size 0))
         (loop (let ((size (read-sequence buffer stream 
                                          :end (min (length buffer) 
                                                    (- chunk-size total-chunk-size)))))
                 (incf total-chunk-size size)
                 (if body 
                     (write-sequence buffer body)
                   (push (subseq buffer 0 size) chunks))
                 (when (= total-chunk-size chunk-size)
                   (return))))
         (read-crlf-line buffer stream))))
    (read-crlf-line buffer stream)
    (or body
        (concatenate-chunks (nreverse chunks) total-size))))

(defun response-read-body (stream length body buffer)
  ;; content-length is known, read whole body directly in a presized buffer
  ;; if body is defined, it should be a stream and we copy the body per buffer to it
  (if (and body buffer)
      (labels ((copy-chunks (read-so-far)
                 (let* ((left-to-read (- length read-so-far))
                        (size (read-sequence buffer stream :end (min (length buffer) left-to-read))))
                   (cond ((< size (length buffer))
                          (write-sequence buffer body :end size))
                         (t
                          (write-sequence buffer body)
                          (copy-chunks (+ read-so-far size)))))))
        (copy-chunks 0)
        body)
    (let ((buffer (make-string length)))
      (read-sequence buffer stream)
      buffer)))

(defun response-read-undefined-body (stream body buffer)
  ;; content-length is unknown, read until eof
  (if body
      (s-utils:copy-stream stream body buffer)
    (let ((total-size 0)
          chunks)
      (loop
       (let ((size (read-sequence buffer stream)))
         (incf total-size size)
         (push (subseq buffer 0 size) chunks)
         (when (< size (length buffer))
           (return))))
      (concatenate-chunks (nreverse chunks) total-size))))

;; authentication support

(defun encode-basic-authorization (username-password-pair)
  (destructuring-bind (username . password) 
      username-password-pair
    (with-output-to-string (out)
      (s-base64::encode-base64-bytes (map 'vector 
                                         #'char-code 
                                         (format nil "~a:~a" username password)) 
                                    out))))

;; connection / server state management

(defvar *connection-keep-alive-timeout* (* 10 60)
  "Keep-Alive connections not used for this amount of time will be force closed")

(defmethod cleanup-old-connections ((http-client-state http-client-state))
  "Force close all kept-alive connections that are too old"
  (with-slots (data) 
      http-client-state
    (let (connections-to-close new-data)
      (loop :for connection :in data :do
            (if (< *connection-keep-alive-timeout* (get-kept-alive-age connection))
                (push connection connections-to-close)
              (push connection new-data)))
      (dolist (connection connections-to-close)
        (close-connection (get-socket connection) :abort t)
        (setf (get-in-use-p connection) nil)
        (setf (get-socket connection) nil))
      (setf data new-data))))

(defmethod get-http-server-state ((http-client-state http-client-state) scheme-host-port)
  (with-slots (data) 
      http-client-state
    (let ((server-state (find scheme-host-port data 
                              :key #'get-scheme-host-port :test #'string-equal)))
      (if server-state
          (setf (get-timestamp server-state) (get-universal-time))
        (push (setf server-state (make-instance 'http-server-state 
                                                :scheme-host-port scheme-host-port))
              data))
      (cleanup-old-connections http-client-state)
      server-state)))

(defun open-socket-stream (scheme host port &key connect-timeout read-timeout write-timeout)
   (ecase scheme
     (:http 
      (s-sysdeps:open-socket-stream host port
                                    :connect-timeout connect-timeout 
                                    :read-timeout read-timeout 
                                    :write-timeout write-timeout))
     (:https 
      #+lispworks (comm:open-tcp-stream host port :ssl-ctx t :timeout connect-timeout :read-timeout read-timeout)
      #-lispworks (error "HTTPS doesn't seem to be supported on your platform"))))

(defun get-open-connection (scheme host port state &key force-new connect-timeout read-timeout write-timeout)
  "Get an open connection to scheme://host:port reusing one from state unless force-new is t - Release after use"
  (if state
      (s-sysdeps:with-process-lock ((get-lock state))
        (let* ((scheme-host-port (format nil "~a://~a:~d" scheme host port))
               (server-state (get-http-server-state state scheme-host-port))
               (connection (get-socket server-state)))
          (cond ((get-in-use-p server-state)
                 ;; we cannot reuse the connection we found since it is in use
                 (values (open-socket-stream scheme host port
                                             :connect-timeout connect-timeout 
                                             :read-timeout read-timeout 
                                             :write-timeout write-timeout)
                         :new 
                         (make-string 4096)))
                ((and connection (open-stream-p connection) (not force-new))
                 ;; we found an open connection that is not in use and are not forced to use a new one
                 ;; so reuse it after marking it in-use and setting the read-timeout
                 (setf (get-in-use-p server-state) t)
                 #+lispworks (setf (stream:stream-read-timeout connection) read-timeout) 
                 (values connection :keep-alive (get-buffer server-state)))
                (t
                 ;; either we didn't find a good connection are were forced to use a new one
                 ;; so we have to open a new one and store it for later
                 (close-connection connection)
                 (setf (get-in-use-p server-state) t)
                 (values (setf (get-socket server-state) (open-socket-stream scheme host port
                                                                             :connect-timeout connect-timeout 
                                                                             :read-timeout read-timeout 
                                                                             :write-timeout write-timeout)) 
                         :new
                         (get-buffer server-state))))))
    ;; without state management, always make new connections
    (values (open-socket-stream scheme host port
                                :connect-timeout connect-timeout 
                                :read-timeout read-timeout 
                                :write-timeout write-timeout) 
            :new 
            (make-string 4096))))

(defun close-connection (connection &key abort)
  "Close a socket connection stream, optionally aborting"
  (when connection 
    (ignore-errors
      (close connection :abort abort))))

(defun release-connection (scheme host port state connection)
  "Release a connection obtained through get-open-connection (either stored in state for reuse or not)"
  (if state
      (s-sysdeps:with-process-lock ((get-lock state))
        (let* ((scheme-host-port (format nil "~a://~a:~d" scheme host port))
               (server-state (get-http-server-state state scheme-host-port)))
          (if (and (eq connection (get-socket server-state))
                   (get-in-use-p server-state))
              (setf (get-in-use-p server-state) nil)
            (close-connection connection))))
    (close-connection connection)))

(defmethod close-all-connections-internal ((http-client-state http-client-state) &key abort)
  (with-slots (data) 
      http-client-state
    (dolist (http-server-state data)
      (close-connection (get-socket http-server-state) :abort abort)
      (setf (get-in-use-p http-server-state) nil)
      (setf (get-socket http-server-state) nil))))
  
(defun close-all-connections (&optional (http-client-state *default-http-client-state*) &key abort)
  "Close all open connections in http-client-state, optionaly aborting them"
  (close-all-connections-internal http-client-state :abort abort))

;; high level HTTP protocol

(defun read-response (stream buffer &optional body (body-expected t))
  "Read an HTTP response, headers and content, from stream"
  (let (response-code response-headers response-body)
    (setf response-code (response-read-code stream buffer)
          response-headers (response-read-headers stream buffer))
    (when (and body-expected (not (= response-code 204)))
      (let* ((content-length-header (cdr (find :content-length response-headers :key #'car)))
             (content-length (when content-length-header 
                               (ignore-errors (parse-integer content-length-header))))
             (transfer-encoding-header (cdr (find :transfer-encoding response-headers :key #'car)))
             (chunked-p (when transfer-encoding-header
                          (search "chunked" transfer-encoding-header :test #'char-equal))))
        (setf response-body (if chunked-p
                                (response-read-chunked-body stream body buffer)
                              (if content-length
                                  (response-read-body stream content-length body buffer)
                                (response-read-undefined-body stream body buffer))))))
    (values response-body
            response-code
            response-headers)))

(defun write-request (stream buffer
                      uri method
                      &key 
                      content content-type content-length 
                      basic-authorization
                      headers)
  "Write an HTTP request, full header and body, to stream"
  (format-http-request-line stream 
                            "~a ~a~@[?~a~] HTTP/1.1" 
                            (string-upcase method) 
                            (or (puri:uri-path uri) "/") 
                            (puri:uri-query uri))
  (format-http-request-line stream "Host: ~a:~d" (puri:uri-host uri) (puri:uri-port uri))
  (format-http-request-line stream "User-Agent: ~a" *http-client-agent*)
  (format-http-request-line stream "Accept: ~a" *http-client-accept*)
  (when basic-authorization
    (format-http-request-line stream "Authorization: Basic ~a" 
                              (encode-basic-authorization basic-authorization)))
  (when (and content content-type) 
    ;; for string content, the content-length is either computed or the specified one is used
    ;; for a stream content, the content-length must be specified by the caller
    (unless content-length (setf content-length (length content)))
    (format-http-request-line stream "Content-Length: ~d" content-length)
    (format-http-request-line stream "Content-Type: ~a" content-type))
  ;; maybe custom headers should be able to override (some of) the above ?
  (loop :for (header-name . header-value) :in headers
        :do (format-http-request-line stream "~a: ~a" header-name header-value))
  (write-http-request-line "" stream)
  ;; content can be a simple string that we use as body
  ;; or content can be an input-stream that we read from per buffer
  (when (and content content-type)
    (etypecase content
      (string (write-sequence content stream :end content-length))
      (stream (s-utils:copy-stream content stream buffer))))
  (finish-output stream))

(defun do-one-request-response (connection 
                                uri method
                                buffer
                                &key 
                                content content-type content-length
                                body
                                basic-authorization
                                headers)
  "Do one HTTP request and response on stream"
  (unless connection (error "Network connection could not be established"))
  (write-request connection buffer uri method 
                 :content content :content-type content-type :content-length content-length 
                 :basic-authorization basic-authorization
                 :headers headers)
  (let ((body-expected (if (eql method :head) nil t)))
    (values-list `(,@(multiple-value-list (read-response connection buffer body body-expected)) ,uri))))

#+lispworks (eval-when (:compile-toplevel :load-toplevel) 
              (require "comm")) ;; to prevent an warnings further on

(defun do-http-request-internal (scheme host port state 
                                 method uri content content-type content-length body basic-authorization headers
                                 connect-timeout read-timeout write-timeout)
  (multiple-value-bind (connection keep-alive buffer)
      ;; state could hold an open (kept alive) connection to host:port
      (get-open-connection scheme host port state
                           :connect-timeout connect-timeout :read-timeout read-timeout :write-timeout write-timeout)
    (flet ((execute-request-response ()
             (values-list `(,@(multiple-value-list 
                               (do-one-request-response connection uri method buffer 
                                                        :content content 
                                                        :content-type content-type 
                                                        :content-length content-length
                                                        :body body
                                                        :basic-authorization basic-authorization
                                                        :headers headers)) 
                            ,keep-alive))))
      (unwind-protect
          (handler-case (execute-request-response)
            ;; the first stream/socket error on the re-used (kept-alive) connection is interpreted 
            ;; as a timeout on the keep-alive, so we close the connection and retry once
            ((or stream-error #+lispworks comm:socket-error) ()
             (release-connection scheme host port state connection)
             (setf keep-alive :new
                   connection (get-open-connection scheme host port state 
                                                   :force-new t :connect-timeout connect-timeout 
                                                   :read-timeout read-timeout :write-timeout write-timeout))
             (execute-request-response)))
        (release-connection scheme host port state connection)))))

;; the user level API 

(defun do-http-request (uri 
                        &key 
                        (method :get) 
                        content content-type content-length
                        body
                        basic-authorization
                        headers
                        proxy
                        (state *default-http-client-state*)
                        connect-timeout
                        read-timeout
                        write-timeout)
  "Execute an HTTP request, returns (VALUES body code headers uri kept-alive-state)"
  (declare (ignore proxy))
  (assert (member method '(:get :put :post :delete :head)))
  (setf uri (puri:parse-uri uri))
  (let* ((scheme (puri:uri-scheme uri))
         (host (or (puri:uri-host uri)))
         (port (or (puri:uri-port uri) 
                   (setf (puri:uri-port uri) (ecase scheme
                                               (:http  80)
                                               (:https 443))))))
    (do-http-request-internal scheme host port state 
                              method uri content content-type content-length body basic-authorization headers
                              connect-timeout read-timeout write-timeout)))

;; our own uri encoding implementation (ascii only, not that efficient, eager to be safe

(defparameter +uri-encode-char-map+
  (let ((map (make-array '(127) :element-type 'boolean)))
    (flet ((allow-range (from to)
             (loop :for i :upfrom (char-code from) :upto (char-code to) 
                   :do (setf (aref map i) t))))
      (allow-range #\0 #\9)
      (allow-range #\A #\Z)
      (allow-range #\a #\z)
      (loop :for char :across "-_.!~*'()" ;; these are the 'mark' characters from the 'unreserved' set
            :do (setf (aref map (char-code char)) t))
      map)))

(defun uri-encode-for-query (string &key (signal-errors t))
  "URI encode string for use as a query parameter value"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
       :for char = (read-char in nil nil)
       :until (null char)
       :do
       (let ((code (char-code char)))
         (cond ((>= code 256) (if signal-errors 
                                  (error "non-ascii char") 
                                (format out "%3F")))
               ((or (>= code 127)
                    (not (aref +uri-encode-char-map+ code)))
                (format out "%~2,'0x" code))
               (t (write-char char out))))))))

(defun uri-decode-for-query (string)
  "URI decode string from a query parameter value"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop 
       :for char = (read-char in nil nil)
       :until (null char)
       :do
       (if (char= char #\%)
           (let ((char1 (read-char in nil nil))
                 (char2 (read-char in nil nil)))
             (if (and char1 char2)
                 (write-char (code-char (parse-integer (map 'string #'identity (list char1 char2)) 
                                                       :radix 16)) 
                             out)
               (error "incomplete escape sequence")))
         (write-char char out))))))

;;;; eof
