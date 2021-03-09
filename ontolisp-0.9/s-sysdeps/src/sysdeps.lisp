;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: base64.lisp,v 1.3 2005/02/07 17:45:41 scaekenberghe Exp $
;;;;
;;;; S-SYSDEPS is an abtraction layer over platform dependent functionality
;;;;
;;;; Copyright (C) 2004-2007 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-sysdeps)

;; loading platform specific dependencies

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

;; managing processes

(defun multiprocessing-capable-p ()
  "Returns t when this implementation is multiprocessing capable"
  #+(or lispworks abcl openmcl allegro sb-thread) t
  #-(or lispworks abcl openmcl allegro sb-thread) nil)

(defun current-process ()
  "Return the object representing the current process"
  #+lispworks mp:*current-process* 
  #+abcl (ext:current-thread)
  #+openmcl ccl:*current-process*
  #+sb-thread sb-thread:*current-thread*
  #+allegro sys:*current-process*
  #-(or lispworks abcl openmcl sb-thread allegro) nil)

(defun kill-process (process)
  "Kill the process represented by the object process"
  #+lispworks (mp:process-kill process)
  #+abcl (ext:destroy-thread process)
  #+openmcl (ccl:process-kill process)
  #+sb-thread (sb-thread:terminate-thread process)
  #+allegro (mp:process-kill process)
  #-(or lispworks abcl openmcl sb-thread allegro) process)

(defun run-process (name function &rest arguments)
  "Create and run a new process with name, executing function on arguments"
  #+lispworks (apply #'mp:process-run-function name '(:priority 3) function arguments)
  #+abcl (ext:make-thread #'(lambda () (apply function arguments)) :name name)
  #+openmcl (apply #'ccl:process-run-function name function arguments)
  #+allegro (apply #'mp:process-run-function name function arguments)
  #+sb-thread (sb-thread:make-thread #'(lambda () (apply function arguments)) :name name)
  #-(or lispworks abcl openmcl allegro sb-thread) 
  (declare (ignore name)) 
  #-(or lispworks abcl openmcl allegro sb-thread)
  (apply function arguments))

(defun all-processes ()
  "Return a list of all processes currently running"
  #+lispworks (mp:list-all-processes)
  #+abcl (ext:mapcar-threads #'identity)
  #+openmcl (ccl:all-processes)
  #+sb-thread (sb-thread:list-all-threads)
  #+allegro sys:*all-processes*
  #-(or lispworks abcl openmcl sb-thread allegro) nil)

;; opening a client TCP/IP socket stream

(defun open-socket-stream (host port &key connect-timeout read-timeout write-timeout)
  "Create and open a bidirectional client TCP/IP socket stream to host:port"
  #+(or abcl openmcl allegro clisp cmu sbcl) (declare (ignore connect-timeout read-timeout write-timeout))
  #+lispworks (comm:open-tcp-stream host port 
                                    :timeout connect-timeout 
                                    :read-timeout read-timeout
                                    :write-timeout write-timeout)
  #+abcl (let ((socket (ext:make-socket host port)))
	   (ext:get-socket-stream socket))
  #+openmcl (ccl:make-socket :remote-host host :remote-port port)
  #+allegro (acl-socket:make-socket :remote-host host :remote-port port 
                                    :type :stream :address-family :internet)
  #+clisp (make-bivalent-stream (socket:socket-connect port host :element-type '(unsigned-byte 8)))
  #+cmu (sys:make-fd-stream (ext:connect-to-inet-socket host port) 
                            :input t :output t :buffering :none)
  #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                                      :type :stream :protocol :tcp)))
           (sb-bsd-sockets:socket-connect socket 
                                          (car 
                                           (sb-bsd-sockets:host-ent-addresses 
                                            (sb-bsd-sockets:get-host-by-name host))) 
                                          port)
           (sb-bsd-sockets:socket-make-stream socket 
                                              :element-type :default
                                              :input t :output t :buffering :none))
  #-(or lispworks abcl openmcl allegro clisp cmu sbcl) 
  (error "Opening a socket stream to ~a:~d not yet ported this lisp system" host port))

;; accessing socket stream properties

(defun get-socket-stream-property (socket-stream property)
  "Get the value of a socket stream property, one of :remote-host :remote-port :local-host :local-port"
  #+lispworks (ecase property
                ((:remote-host :remote-port) (multiple-value-bind (address port)
                                                 (comm:socket-stream-peer-address socket-stream)
                                               (if (eql property :remote-host)
                                                   (when address (comm:ip-address-string address))
                                                 port)))
                ((:local-host :local-port) (multiple-value-bind (address port)
                                               (comm:socket-stream-address socket-stream)
                                             (if (eql property :local-host)
                                                 (when address (comm:ip-address-string address))
                                               port))))
  #+clisp (ecase property
            ((:remote-host :remote-port) (multiple-value-bind (host port)
                                             (socket:socket-stream-peer (get-native-stream socket-stream))
                                           (if (eql property :remote-host)
                                               host
                                             port)))
            ((:local-host :local-port) (multiple-value-bind (host port)
                                           (socket:socket-stream-local (get-native-stream socket-stream))
                                         (if (eql property :local-host)
                                             host
                                           port))))
  #-(or lispworks clisp) 
  (declare (ignore socket-stream property))
  #-(or lispworks clisp) 
  nil)

;; implementing a standard TCP/IP server

#+(or sb-thread cmu)
(defvar *server-processes* '()
  "The list of processes created by S-SYSDEPS")

(defun start-standard-server (&key port name connection-handler)
  "Start a server process with name, listening on port, delegating to connection-handler with stream as argument"
  #+lispworks (comm:start-up-server
               :function #'(lambda (socket-handle)
                             (let ((client-stream (make-instance 'comm:socket-stream
                                                                 ;; maybe specify a read timeout...
                                                                 :socket socket-handle
                                                                 :direction :io
                                                                 :element-type 'base-char)))
                               (funcall connection-handler client-stream)))
               :service port
               :announce t
               :wait t
               :process-name name)
  #+abcl (ext:make-thread
	  #'(lambda ()
	      (let ((server-socket (ext:make-server-socket port)))
		(unwind-protect
		     (loop
			(let* ((client-socket (ext:socket-accept server-socket))
			       (client-stream (ext:get-socket-stream client-socket)))
			  (funcall connection-handler client-stream)))
		  (ext:server-socket-close server-socket))))
	  :name name)
  #+openmcl (ccl:process-run-function
             name
             #'(lambda ()
                 (let ((server-socket (ccl:make-socket :connect :passive
                                                       :local-port port
                                                       :reuse-address t)))
                   (unwind-protect
                       (loop 
                        (let ((client-stream (ccl:accept-connection server-socket)))
                          (funcall connection-handler client-stream))) 
                     (close server-socket)))))
  #+allegro (mp:process-run-function
	     name
	     #'(lambda ()
		 (let ((server-socket (acl-socket:make-socket :connect :passive 
							      :local-port port)))
		   (unwind-protect
                       (loop
                        (let ((client-stream (acl-socket:accept-connection server-socket)))
                          (funcall connection-handler client-stream)))
                     (close server-socket)))))
  #+sb-thread (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                                            :type :stream :protocol :tcp))
                     (handler-fn (lambda (fd)
                                   (declare (ignore fd))
                                   (let ((stream
                                          (sb-bsd-sockets:socket-make-stream
                                           (sb-bsd-sockets:socket-accept socket)
                                           :element-type 'character
                                           :input t
                                           :output t
                                           :buffering :none)))
                                     (funcall connection-handler stream)))))
                (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
                (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
                (sb-bsd-sockets:socket-listen socket 15)
                (push (list name 
                            socket
                            (sb-sys:add-fd-handler 
                             (sb-bsd-sockets:socket-file-descriptor socket)
                             :input handler-fn)) 
                      *server-processes*))
  #+cmu (let* ((socket (ext:create-inet-listener port :stream :reuse-address t
                                                 :backlog 15))
               (handler-fn (lambda (fd)
                             (declare (ignore fd))
                             (let ((stream (sys:make-fd-stream
                                            (ext:accept-tcp-connection socket)
                                            :input t :output t
                                            :element-type 'character
                                            :buffering :none)))
                               (funcall connection-handler stream)))))
          (push (list name 
                      socket
                      (sys:add-fd-handler socket :input handler-fn))
                *server-processes*))
  #+clisp (declare (ignore name))
  ;; Implementation Limitation: in CLISP this becomes a blocking single-threaded server
  #+clisp (let ((server-socket (socket:socket-server port :backlog 15)))
            (unwind-protect 
                (loop
                 (let ((client-socket (socket:socket-accept server-socket :element-type '(unsigned-byte 8))))
                   (funcall connection-handler (make-bivalent-stream client-socket))))
              (socket:socket-server-close server-socket))
            nil)
  #-(or lispworks abcl openmcl allegro sb-thread cmu clisp) 
  (error "Starting a standard socket named ~s on port ~d using handler ~s not yet ported to this lisp system"
         name port connection-handler)) 

#+(or sb-thread cmu)
(defun stop-server (name)
  "Stop a named server"
  #+sb-thread (progn
                (destructuring-bind (name socket handler)
                    (assoc name *server-processes* :test #'string=)
                  (declare (ignore name))
                  (sb-sys:remove-fd-handler handler)
                  (sb-bsd-sockets:socket-close socket))
                (setf *server-processes* (delete name *server-processes*
                                                 :key #'car :test #'string=)))
  #+cmu (progn
          (destructuring-bind (name socket handler)
              (assoc name *server-processes* :test #'string=)
            (declare (ignore name))
            (sys:remove-fd-handler handler)
            (unix:unix-close socket))
          (setf *server-processes* (delete name *server-processes*
                                           :key #'car :test #'string=)))
  name)

;; working with process locks

(defun make-process-lock (name)
  "Create a named process lock object"
  #+lispworks (mp:make-lock :name name)
  #+abcl (ext:make-thread-lock)
  #+openmcl (ccl:make-lock name)
  #+allegro (mp:make-process-lock :name name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #-(or lispworks abcl openmcl allegro sb-thread) 
  (declare (ignore name))
  #-(or lispworks abcl openmcl allegro sb-thread) 
  nil)

(defmacro with-process-lock ((lock) &body body)
  "Execute body wih the process lock grabbed, wait otherwise"
  ;; maybe it is safer to always use a timeout: 
  ;; `(mp:with-lock (,lock (format nil "Waiting for ~s" (lock-name ,lock)) 5) ,@body)
  ;; if the lock cannot be claimed in 5s, nil is returned: test it and throw a condition ?
  #+lispworks `(mp:with-lock (,lock) ,@body)
  #+abcl `(ext:with-thread-lock (,lock) ,@body)
  #+openmcl `(ccl:with-lock-grabbed (,lock) ,@body)
  #+allegro `(mp:with-process-lock (,lock) ,@body)
  #+sb-thread `(sb-thread:with-recursive-lock (,lock) ,@body)
  #-(or lispworks abcl openmcl allegro sb-thread) 
  (declare (ignore lock))
  #-(or lispworks abcl openmcl allegro sb-thread) 
  `(progn ,@body))

;;;; eof
