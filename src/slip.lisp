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

;;; SLIP encoding and decoding routines

(in-package "COM.WILDORA.LW-SLIP")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +end+     #O300)
  (defconstant +esc+     #O333)
  (defconstant +esc-end+ #O334)
  (defconstant +esc-esc+ #O335))

(defmacro make-byte-vector (&optional (size 128))
  `(make-array ,size
               :element-type '(unsigned-byte 8)
               :fill-pointer 0
               :adjustable t
               :single-thread t))

(define-condition decode-error (error)
  ())

;; States:
;;   expect-end
;;   read-next
;;   unescape-next
(defun make-decoder ()
  (let ((decode-buffer (make-byte-vector))
        (state nil))
    (labels ((reset ()
               (setf (fill-pointer decode-buffer) 0
                     state 'expect-end)))
      (lambda (input-buffer start end)
        (handler-bind ((decode-error
                        (lambda (condition)
                          (declare (ignore condition))
                          (reset))))
          (loop :with packets := ()
                :for i :from start :below end
                :for byte := (aref input-buffer i) :do
                (case state
                  (read-next
                   (case byte
                     (#.+esc+
                      (setf state 'unescape-next))
                     (#.+end+
                      (when (plusp (length decode-buffer))
                        (push (copy-seq decode-buffer)
                              packets))
                      (reset))
                     (otherwise
                      (vector-push-extend byte decode-buffer))))
                  (unescape-next
                   (vector-push-extend (case byte
                                         (#.+esc-end+ +end+)
                                         (#.+esc-esc+ +esc+)
                                         (otherwise
                                          (error 'decode-error)))
                                       decode-buffer)
                   (setf state 'read-next))
                  (expect-end
                   (unless (eql byte +end+)
                     (error 'decode-error)))
                  (otherwise
                   (when (eql byte +end+)
                     (setf state 'read-next))))
                :finally (return (nreverse packets))))))))

(defun decode-packets (decoder buffer
                       &key (start 0) (end (length buffer)))
  (funcall decoder buffer start end))

(defun encode-packet (buffer)
  (let ((encode-buffer (make-byte-vector)))
    (vector-push-extend +end+ encode-buffer)
    (loop :for byte :of-type (unsigned-byte 8)
          :across buffer :do
          (case byte
            (#.+end+
             (vector-push-extend +esc+ encode-buffer)
             (vector-push-extend +esc-end+ encode-buffer))
            (#.+esc+
             (vector-push-extend +esc+ encode-buffer)
             (vector-push-extend +esc-esc+ encode-buffer))
            (t
             (vector-push-extend byte encode-buffer))))
    (vector-push-extend +end+ encode-buffer)
    encode-buffer))
