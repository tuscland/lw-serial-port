(defpackage "COM.WILDORA.SERIAL-STREAM"
  (:nicknames "SERIAL-STREAM")
  (:export
   "OPEN-SERIAL-STREAM"
   "SERIAL-STREAM-ERROR"
   "SERIAL-STREAM-SPEED"
   "SERIAL-PORT-NAMES"))

(in-package "COM.WILDORA.SERIAL-STREAM")

(fli:define-c-struct (_commtimeouts
                      (:foreign-name "_COMMTIMEOUTS"))
  (read-interval-timeout win32:dword)
  (read-total-timeout-multiplier win32:dword)
  (read-total-timeout-constant win32:dword)
  (write-total-timeout-multiplier win32:dword)
  (write-total-timeout-constant win32:dword))

(fli:define-foreign-function (%set-comm-timeouts
                              "SetCommTimeouts")
    ((handle win32:handle)
     (timeouts (:pointer (:struct _commtimeouts))))
  :result-type cw:boolean-result)

(defun set-comm-timeouts (handle
                          read-interval-timeout
                          read-total-timeout-multiplier
                          read-total-timeout-constant
                          &optional (write-total-timeout-multiplier 0)
                                    (write-total-timeout-constant 0))
  (fli:with-dynamic-foreign-objects ((timeouts (:struct _commtimeouts)
                                      :fill 0))
    (fli:with-foreign-slots ((rit read-interval-timeout)
                             (rttm read-total-timeout-multiplier)
                             (rttc read-total-timeout-constant)
                             (wttm write-total-timeout-multiplier)
                             (wttc write-total-timeout-constant)) timeouts
      (setf rit read-interval-timeout
            rttm read-total-timeout-multiplier
            rttc read-total-timeout-constant
            wttm write-total-timeout-multiplier
            wttc write-total-timeout-constant))
    (%set-comm-timeouts handle timeouts)))

(fli:define-foreign-function (get-comm-state
                              "GetCommState")
    ((handle win32:handle)
     (dcb (:pointer win32:dcb)))
  :result-type cw:boolean-result)

(fli:define-foreign-function (build-comm-dcb
                              "BuildCommDCB" :dbcs)
    ((definition (:reference-pass :ef-wc-string))
     (dcb (:pointer win32:dcb)))
  :result-type cw:boolean-result)

(fli:define-foreign-function (set-comm-state
                              "SetCommState")
    ((handle win32:handle)
     (dcb (:pointer win32:dcb)))
  :result-type cw:boolean-result)


;; From: http://msdn.microsoft.com/en-us/library/ms810467.aspx#serial_topic4
(defun serial-read (handle buffer number-of-bytes-to-read timeout) ; timeout is in ms
  (let (waiting-on-read)
    (fli:with-dynamic-foreign-objects ((number-of-bytes-read win32:dword
                                        :fill 0)
                                       (overlapped cw:overlapped
                                        :fill 0))
      (cw:with-open-handle (event (cw:create-event nil t))
        (setf (fli:foreign-slot-value overlapped 'cw:event-handle) event)
        (unless waiting-on-read
          (handler-case (cw:read-file handle
                                      buffer
                                      number-of-bytes-to-read
                                      number-of-bytes-read
                                      overlapped)
            (cw:error-io-pending (error)
              (setf waiting-on-read t))))
        (when waiting-on-read
          (case (cw:wait-for-single-object event timeout)
            (#.cw:WAIT_OBJECT_0
             (cw:get-overlapped-result handle overlapped number-of-bytes-read nil)
             (setf waiting-on-read nil))
            (#.cw:WAIT_TIMEOUT
             nil)
            (t
             (cw:win32-error)))))
      (unless waiting-on-read
        (fli:dereference number-of-bytes-read)))))

(defun serial-write (handle buffer number-of-bytes-to-write timeout) ; timeout is in ms
  (fli:with-dynamic-foreign-objects ((number-of-bytes-written win32:dword
                                      :fill 0)
                                     (overlapped cw:overlapped
                                      :fill 0))
    (cw:with-open-handle (event (cw:create-event nil t))
      (setf (fli:foreign-slot-value overlapped 'cw:event-handle) event)
      (handler-case (cw:write-file handle
                                   buffer
                                   number-of-bytes-to-write
                                   number-of-bytes-written
                                   overlapped)
        (cw:error-io-pending (error)
          (case (cw:wait-for-single-object event timeout)
            (#.cw:WAIT_OBJECT_0
             (cw:get-overlapped-result handle overlapped number-of-bytes-written nil))
            (#.cw:WAIT_TIMEOUT
             (do-nothing))
            (t
             (cw:win32-error))))))
    (fli:dereference number-of-bytes-written)))

(defclass serial-stream
          (stream:fundamental-binary-input-stream
           stream:fundamental-binary-output-stream)
  ((port-name
    :initform nil
    :initarg :port-name
    :reader serial-stream-port-name)
   (handle
    :initform (error "HANDLE must be specified.")
    :initarg :handle
    :reader serial-stream-handle)))

(defmethod stream-element-type ((self serial-stream))
  '(unsigned-byte 8))

(defmethod stream:stream-read-byte ((self serial-stream))
  (fli:with-dynamic-foreign-objects ((buffer :unsigned-byte))
    (when-let (number-of-bytes-read (serial-read (serial-stream-handle self)
                                                 buffer
                                                 (fli:size-of :unsigned-byte)
                                                 500))
      ;; FIXME: handle zero bytes read like in write function?
      (when (plusp number-of-bytes-read)
        (fli:dereference buffer)))))

(defmethod stream:stream-write-sequence ((self serial-stream) sequence start end)
  (let ((length (- end start)))
    (fli:with-dynamic-foreign-objects ((buffer :unsigned-byte
                                        :nelems length))
      (fli:replace-foreign-array buffer sequence
                                 :start1 0 :end1 length
                                 :start2 start :end2 end)
      (if (zerop
           (serial-write (serial-stream-handle self)
                         buffer
                         (* length (fli:size-of :unsigned-byte))
                         500))
          (error 'end-of-file :stream self)
        sequence))))

(defmethod close ((self serial-stream) &key abort)
  (declare (ignore abort))
  (with-slots (handle) self
    (cw:cancel-io handle)
    (cw:close-handle handle))
  (call-next-method))

(defmethod (setf serial-stream-speed) (speed (self serial-stream))
  (with-slots (handle) self
    (set-comm-options handle speed)))

(defun set-comm-options (handle speed)
  (fli:with-dynamic-foreign-objects ((dcb win32:dcb
                                      :fill 0
                                      :size-slot 'win32::dcblength))
    (get-comm-state handle dcb)
    (build-comm-dcb (format nil "baud=~A parity=N data=8 stop=1" speed)
                    dcb)
    #+nil
    (fli:with-foreign-slots ((baudrate win32::baudrate)
                             (parity win32::parity)
                             (bytesize win32::bytesize)
                             (stopbits win32::stopbits)) dcb
        (setf baudrate speed
              parity win32:NOPARITY
              bytesize 8
              stopbits win32:ONESTOPBIT))
    #+nil
    (setf (win32:lpdcb-misc-flag-field dcb 'win32::fabortonerror) 1
          (win32:lpdcb-misc-flag-field dcb 'win32::fbinary) 1
          (win32:lpdcb-misc-flag-field dcb 'win32::finx) 0
          (win32:lpdcb-misc-flag-field dcb 'win32::foutxdsrflow) 0
          (win32:lpdcb-misc-flag-field dcb 'win32::foutxctsflow) 0
          (win32:lpdcb-misc-flag-field dcb 'win32::fnull) 0
;          (win32:lpdcb-misc-flag-field dcb 'win32::frtscontrol) 0
          (win32:lpdcb-misc-flag-field dcb 'win32::ftxcontinueonxoff) 1
;          (win32:lpdcb-misc-flag-field dcb 'win32::fdtrcontrol) 0
          )
    (set-comm-state handle dcb)
    (set-comm-timeouts handle 0 10 0 10 100)))

(defun open-serial-stream (port-name speed)
  (let ((handle (cw:create-file port-name
                                (logior cw:GENERIC_READ cw:GENERIC_WRITE)
                                0
                                nil
                                cw:OPEN_EXISTING
                                cw:FILE_FLAG_OVERLAPPED
                                0)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (cw:close-handle handle))))
      (set-comm-options handle speed)
      (make-instance 'serial-stream
                     :handle handle
                     :port-name port-name))))


;; Serial port listing

(defun serial-port-names ()
  (ignore-errors
      (mapcar #'cdr (win32:collect-registry-values 
                     "HARDWARE\\DEVICEMAP\\SERIALCOMM"
                     :root :local-machine))))
