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

;;; Low Level Serial Port Access

(in-package "COM.WILDORA.LW-SERIAL-DEVICE")

;;;; Embedded Module

(define-action "When Starting Image" "Install Serial Port Embedded Module"
               'install-embedded-module)

(defun install-embedded-module ()
  (fli:install-embedded-module :serial))

(unless (delivered-image-p)
  (install-embedded-module))


;;;; Foreign Language Interface

(fli:define-foreign-function serial-sizeof-struct-termios
    ()
  :result-type :size-t)

(defun throw-io-error (description)
  (error 'io-error
         :format-control "Serial Device error occured: \"~A\""
         :format-arguments (list description)))

(defun check-result (result)
  (when (minusp result)
    (throw-io-error
     (strerror (errno-value))))
  result)

(fli:define-foreign-converter
    serial-result ()
    object
  :foreign-type :int
  :foreign-to-lisp `(check-result ,object))

(fli:define-foreign-function serial-open
    ((port-name (:reference-pass :ef-mb-string))
     (original-port-attributes :pointer))
  :result-type serial-result)

(fli:define-foreign-function serial-close
    ((fd :int)
     (original-port-attributes :pointer))
  :result-type serial-result)

(fli:define-foreign-function serial-set-options
    ((fd :int)
     (speed :int)
     (parity :int))
  :result-type serial-result)

(fli:define-foreign-function serial-wait-for-input
    ((fd :int)
     (timeout-s :int)
     (timeout-µs :int))
  :result-type serial-result)

(fli:define-foreign-function serial-read
    ((fd :int)
     (buffer :pointer)
     (length :size-t))
  :result-type serial-result)

(fli:define-foreign-function serial-write
    ((fd :int)
     (buffer :pointer)
     (length :size-t))
  :result-type serial-result)

(fli:define-foreign-function strerror
    ((errno :int))
  :result-type (:reference-return
                (:ef-mb-string :limit 256)))


;;;; Stream Class

(defclass serial-device-stream (stream:fundamental-binary-input-stream
                                stream:fundamental-binary-output-stream)
  ((port-name
    :initform nil
    :initarg :port-name
    :reader serial-device-stream-port-name)
   (fd
    :initform (error "FILE-DESCRIPTOR must be specified.")
    :initarg :file-descriptor
    :reader serial-device-stream-fd)
   (original-port-attributes
    :initarg :original-port-attributes
    :reader serial-device-stream-original-port-attributes)))

(defmethod wait-for-input ((self serial-device-stream))
  (plusp (serial-wait-for-input (serial-device-stream-fd self)
                                0 ;s
                                200000))) ;µs

(defmethod stream-element-type ((self serial-device-stream))
  '(unsigned-byte 8))

(defmethod stream:stream-read-sequence ((self serial-device-stream) sequence start end)
  (let ((length (- end start)))
    (fli:with-dynamic-foreign-objects
        ((buffer :unsigned-byte :nelems length))
      (let* ((number-of-bytes-read
              (serial-read (serial-device-stream-fd self)
                           buffer
                           (* length #.(fli:size-of :unsigned-byte)))))
        (fli:replace-foreign-array sequence buffer
                                   :start1 start :end1 end
                                   :start2 0 :end2 number-of-bytes-read)
        (+ start number-of-bytes-read)))))

(defmethod stream:stream-write-sequence ((self serial-device-stream) sequence start end)
  (let ((length (- end start)))
    (fli:with-dynamic-foreign-objects
        ((buffer :unsigned-byte :nelems length))
      (fli:replace-foreign-array buffer sequence
                                 :start1 0 :end1 length
                                 :start2 start :end2 end)
      ;; TODO: handle the case when there is less bytes written than
      ;; expected.
      (if (zerop
           (serial-write (serial-device-stream-fd self)
                         buffer
                         (* length #.(fli:size-of :unsigned-byte))))
          (throw-io-error "Unable to write on device.")
        sequence))))

(defmethod close ((self serial-device-stream) &key abort)
  (declare (ignore abort))
  (with-slots (fd original-port-attributes) self
    (ignore-errors
      (serial-close fd original-port-attributes))
    (fli:free original-port-attributes))
  (call-next-method))

(defmethod (setf baudrate) (speed (self serial-device-stream))
  (serial-set-options (serial-device-stream-fd self)
                      speed
                      0))

(defun open-stream (port-name &optional (speed 9600))
  (let* ((original-port-attributes (fli:malloc :type :unsigned-byte
                                               :nelems (serial-sizeof-struct-termios)))
         (fd (serial-open port-name original-port-attributes)))
    (handler-bind ((io-error (lambda (condition)
                               (declare (ignore condition))
                               (serial-close fd original-port-attributes)
                               (fli:free original-port-attributes))))
      (when (serial-set-options fd speed 0)
        (make-instance 'serial-device-stream
                       :file-descriptor fd
                       :original-port-attributes original-port-attributes
                       :port-name port-name)))))

(let ((device-names))
  (declare (special device-names))

  (fli:define-foreign-callable serial-enumerate-device-names-callback
      ((name (:reference-return (:ef-mb-string
                                 :null-terminated-p t
                                 :external-format :utf-8))))
    (push name device-names))

  (fli:define-foreign-function serial-enumerate-device-names
      ((callback :pointer)))

  (defun device-names ()
    (setf device-names nil)
    (serial-enumerate-device-names
     (fli:make-pointer :symbol-name 'serial-enumerate-device-names-callback))

    ;; TODO: remove this in the future
    (remove "usb"
            device-names
            :test-not #'search)))
