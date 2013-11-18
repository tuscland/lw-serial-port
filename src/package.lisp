;;;; -*- encoding: utf-8 -*-

(in-package "CL-USER")

(defpackage "COM.WILDORA.LW-SLIP"
  (:nicknames "SLIP")
  (:export
   "MAKE-DECODER"
   "DECODE-PACKETS"
   "ENCODE-PACKET"
   "DECODE-ERROR"))

(defpackage "COM.WILDORA.LW-SERIAL-DEVICE"
  (:nicknames "SERIAL-DEVICE")
  (:export
   "OPEN-STREAM"
   "BAUDRATE"
   "DEVICE-NAMES"
   "WAIT-FOR-INPUT"))

(defpackage "COM.WILDORA.LW-SERIAL-RESPONDER"
  (:nicknames "SERIAL-RESPONDER")
  (:export
   "START"
   "STOP"
   "RUNNINGP"
   "WRITE-BUFFER"
   "SET-BAUDRATE"
   "DEVICE-NAMES")
  (:import-from "COM.WILDORA.LW-SERIAL-DEVICE"
   "DEVICE-NAMES"))
