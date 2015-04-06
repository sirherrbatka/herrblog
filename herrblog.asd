(ql:quickload 'hunchentoot)
(ql:quickload 'cl-markup)
(ql:quickload 'clobber)
(ql:quickload 'cl-ppcre)

(defpackage #:herrblog.asdf
  (:use #:cl #:asdf))

(defpackage :herrblog
  (:use :common-lisp
        :hunchentoot
        :cl-markup
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
  :components ((:file "src/common/method-combinations")
               (:file "src/common/exceptions")
               (:file "src/common/utility")
               (:file "src/common/generic-functions")
               (:file "src/common/common-macros")
               (:file "src/common/base-classes")
               (:file "src/model/spam-filter")
               (:file "src/model/post-parser")
               (:file "src/model/posts-container")
               (:file "src/model/transactions")
               (:file "src/model/post")
               (:file "src/html/constants")
               (:file "src/html/templates")
               (:file "src/html/generators")
               (:file "src/authentication")
               (:file "src/global-definitions")
               (:file "src/network/handlers")
               (:file "src/interfaces/repl")))
