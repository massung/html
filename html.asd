(defpackage :html-asd
  (:use :cl :asdf))

(in-package :html-asd)

(defsystem :html
  :name "html"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "HTML Rendering for Common Lisp."
  :serial t
  :components ((:file "html"))
  :depends-on ())
