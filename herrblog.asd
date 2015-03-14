(ql:quickload 'hunchentoot)
(ql:quickload 'cl-markup)
(ql:quickload 'parenscript)
(ql:quickload 'clobber)
(ql:quickload 'cl-ppcre)

(defpackage #:herrblog.asdf
  (:use #:cl #:asdf))

(defpackage :blog
  (:use :common-lisp
        :hunchentoot
        :cl-markup
        :parenscript
        :clobber
        :cl-ppcre)
  (:export :standard-menu))

(in-package #:herrblog.asdf)

(defsystem herrblog
  :name "blog written in common lisp"
  :version "0.0.0"
  :license "MIT"
  :author "Marek Kochanowicz (aka shka)"
  :maintainer "Marek Kochanowicz (aka shka)"
  :serial T
  :components ((:file "src/model/post-parser")
               (:file "src/html/style")
               (:file "src/authentication")
               (:file "src/common/generic-functions")
               (:file "src/common/common-macros")
               (:file "src/common/base-classes")
               (:file "src/model/posts-container")
               (:file "src/model/post")
               (:file "src/global-definitions")
               (:file "src/html/templates")
               (:file "src/html/constants")
               (:file "src/model/transactions")
               (:file "src/html/generators")
               (:file "src/network/handlers")))
