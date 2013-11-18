;;;; -*- encoding: utf-8 -*-

(in-package "CL-USER")

#+macosx
(setf scm:*c-default-options*
      "-Os -dynamiclib -fno-common -mmacosx-version-min=10.5 -framework IOKit -framework CoreFoundation")

#+mswindows
(load (current-pathname "../core-win/defsystem"))

(defsystem "COM.WILDORA.SERIAL-PORT"
 (:default-pathname "src")
 :members ("package"

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
