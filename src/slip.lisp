;;;; -*- encoding: utf-8 -*-

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
