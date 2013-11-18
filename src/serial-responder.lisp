;;;; -*- encoding: utf-8 -*-

(in-package "COM.WILDORA.LW-SERIAL-RESPONDER")

;;;; Serial Port Responder

(defmacro %device-stream (&optional responder)
  `(mp:process-property 'stream ,responder))

(defmacro %runningp (&optional responder)
  `(mp:process-property 'runningp ,responder))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun responder-loop (stream read-callback events-callback)
  (let ((buffer (make-array 1024
                            :element-type '(unsigned-byte 8)
                            :initial-element 0
                            :single-thread t)))
    (handler-case
        (while (%runningp)
          (serial-device:wait-for-input stream)
          (let ((end (read-sequence buffer stream)))
            (funcall read-callback
                     (subseq buffer 0 end))))
      (error (error)
        (funcall events-callback :error error)))))

(defun start (device-path baudrate read-callback events-callback)
  (let* ((waiting-process (mp:get-current-process))
         (stream (serial-device:open-stream device-path baudrate))
         (responder (mp:process-run-function
                     (format nil "Serial I/O - ~A" device-path)
                     '()
                     (lambda ()
                       (setf (%device-stream) stream
                             (%runningp) t)
                       (mp:process-poke waiting-process)
                       (unwind-protect
                           (responder-loop stream read-callback events-callback)
                         (progn
                           (close stream)
                           (funcall events-callback :cleanup nil)))))))
    (mp:process-wait-local nil (lambda ()
                                 (%runningp responder)))
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