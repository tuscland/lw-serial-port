;;;; -*- encoding: utf-8 -*-

(in-package "CL-USER")

(load-all-patches)

(load (current-pathname "defsystem"))

(compile-system "COM.WILDORA.SERIAL-PORT" :load t)
