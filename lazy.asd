(defpackage :lazy-asd
  (:use :cl :asdf))

(in-package :lazy-asd)

(defsystem :lazy
  :name "lazy"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Lazy forms for Common Lisp."
  :serial t
  :components ((:file "lazy")))
