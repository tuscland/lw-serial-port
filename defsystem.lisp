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

#+macosx
(setf scm:*c-default-options*
      "-Os -dynamiclib -fno-common -mmacosx-version-min=10.5 -framework IOKit -framework CoreFoundation")

#+mswindows
(load (current-pathname "../core-win/defsystem"))

(defsystem "COM.WILDORA.SERIAL-PORT"
 (:default-pathname "src")
 :members ("package"

           "serial-device"

           ("macosx/serial"
            :type :c-file
            :internal-dynamic-module :serial
            :features :macosx)
           ("macosx/serial-device"
            :features :macosx)

           ("CORE-WIN"
            :type :system
            :features :mswindows)
           ("mswindows/serial-device"
            :features :mswindows)
           ("mswindows/foreign-templates"
            :features :mswindows)

           "slip"
           "serial-responder")
 :rules ((:in-order-to :compile :all
          (:requires (:load :previous)))))
