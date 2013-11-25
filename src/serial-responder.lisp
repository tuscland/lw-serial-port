;;;; -*- encoding: utf-8; mode: LISP; syntax: COMMON-LISP; indent-tabs-mode: nil -*-

;;; LispWorks Serial Port.
;;; Copyright (c) 2013, Camille Troillard. All rights reserved.

;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an "AS
;;; IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
;;; express or implied.  See the License for the specific language
;;; governing permissions and limitations under the License.

;; High Level Serial Port Access

(in-package "COM.WILDORA.LW-SERIAL-RESPONDER")

(defmacro %device-stream (&optional responder)
  `(mp:process-property 'stream ,responder))

(defmacro %runningp (&optional responder)
  `(mp:process-property 'runningp ,responder))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun status (responder)
  (cond
   ((not (null (%device-stream responder)))
    :ok)
   ((mp:process-alive-p responder)
    :pending)))

(defun responder-loop (read-callback events-callback)
  (setf (%runningp) t)
  (unwind-protect
      (let ((stream (%device-stream))
            (buffer (make-array 1024
                                :element-type '(unsigned-byte 8)
                                :initial-element 0
                                :single-thread t)))
        (handler-case
            (while (%runningp)
              (serial-device:wait-for-input stream)
              (let ((end (read-sequence buffer stream)))
                (handler-case
                    (funcall read-callback
                             (subseq buffer 0 end))
                  (error (condition)
                    (funcall events-callback :error condition)))))
          ;; TODO: handle specifically serial-device:io-error
          (serial-device:io-error (condition)
            (funcall events-callback :error condition))))
    (close stream)
    (funcall events-callback :cleanup nil)))

(defun start (device-path baudrate read-callback events-callback)
  (let* ((stream (serial-device:open-stream device-path baudrate))
         (responder (mp:process-run-function
                     (format nil "Serial I/O - ~A" device-path)
                     '()
                     #'responder-loop
                     read-callback
                     events-callback)))
    (setf (%device-stream responder) stream)
    responder))

(defun stop (responder)
  (when (mp:process-alive-p responder)
    (setf (%runningp responder) nil)
    (mp:process-join responder)))

(defun runningp (responder)
  (and (mp:process-alive-p responder)
       (%device-stream responder)))

(defun write-buffer (responder buffer)
  (write-sequence buffer
                  (%device-stream responder)))

(defun set-baudrate (responder baudrate)
  (setf (serial-device:baudrate
         (%device-stream responder))
        baudrate))
