;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-base64.lisp,v 1.1.1.1 2004/06/09 09:02:41 scaekenberghe Exp $
;;;;
;;;; An HTTP/HTTPS Benchmark Tool based on S-HTTP-CLIENT, along the lines of ab
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-http-client)

(defun benchmark-one (uri &key (number-of-calls 16) separate-states id stream)
  (let ((state (if separate-states 
                   (make-http-client-state)
                 *default-http-client-state*))
        (start (get-internal-real-time))
        stop)
    (loop :for i :from 1 :to number-of-calls :do
          (multiple-value-bind (contents code headers uri keep-alive)
              (do-http-request uri :state state)
            (declare (ignore headers))
            (when stream
              (format stream ";; S-HTTP-CLIENT::BENCHMARK [~@[~a:~]~d] ~s ~a (~d bytes) ~s~%"
                      id i uri code (length contents) keep-alive))))
    (close-all-connections state)
    (setf stop (get-internal-real-time))
    (- stop start)))

(defun benchmark (uri &key (number-of-calls 16) (concurrency 1) separate-states (stream *standard-output*) trace)
  (format stream ";; S-HTTP-CLIENT::BENCHMARK ~s~%" uri)
  (if (eql concurrency 1)
      (let ((runtime (benchmark-one uri 
                                    :number-of-calls number-of-calls 
                                    :separate-states separate-states
                                    :stream (when trace *standard-output*))))
        (format stream ";; S-HTTP-CLIENT::BENCHMARK ~d requests took ~d ms or ~d request/s~%"
                number-of-calls runtime (/ 1000.0 (/ runtime number-of-calls))))
    (let ((calls-per-process (floor number-of-calls concurrency))
          (lock (s-sysdeps:make-process-lock "benchmark"))
          (start (get-internal-real-time))
          stop
          runtime
          runtimes)
      (loop :for id :below concurrency :do
            (format stream ";; S-HTTP-CLIENT::BENCHMARK spawning ~d~%" id)
            (s-sysdeps:run-process (format nil "benchmark-process-~d" id)
                                   #'(lambda (my-id)
                                       (let ((runtime (benchmark-one uri
                                                                     :id my-id
                                                                     :number-of-calls calls-per-process 
                                                                     :separate-states separate-states
                                                                     :stream (when trace stream))))
                                         (s-sysdeps:with-process-lock (lock)
                                           (push runtime runtimes)
                                           (setf stop (get-internal-real-time)))))
                                   id))
      (loop :while (s-sysdeps:with-process-lock (lock) (< (length runtimes) concurrency))
            :do (sleep 1))
      (setf runtime (- stop start))
      (format stream ";; S-HTTP-CLIENT::BENCHMARK ~d requests took ~d ms or ~d request/s (runtime)~%"
              number-of-calls runtime (/ 1000.0 (/ runtime number-of-calls)))
      (setf runtime (reduce #'+ runtimes))
      (format stream ";; S-HTTP-CLIENT::BENCHMARK ~d requests took ~d ms or ~d request/s (totaltime)~%"
              number-of-calls runtime (/ 1000.0 (/ runtime number-of-calls))))))

;;;; eof
