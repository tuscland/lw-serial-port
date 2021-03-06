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
   "IO-ERROR"
   "OPEN-STREAM"
   "BAUDRATE"
   "DEVICE-NAMES"
   "WAIT-FOR-INPUT"
   "+COMMON-BAUDRATES+"))

(defpackage "COM.WILDORA.LW-SERIAL-RESPONDER"
  (:nicknames "SERIAL-RESPONDER")
  (:export
   "STATUS"
   "START"
   "STOP"
   "RUNNINGP"
   "WRITE-BUFFER"
   "SET-BAUDRATE"
   "DEVICE-NAMES")
  (:import-from "COM.WILDORA.LW-SERIAL-DEVICE"
   "DEVICE-NAMES"))
