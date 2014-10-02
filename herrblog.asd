(ql:quickload 'hunchentoot)
(ql:quickload 'cl-markup)
(ql:quickload 'parenscript)
(ql:quickload 'clobber)
(ql:quickload 'split-sequence)

(defpackage #:herrblog.asdf
  (:use #:cl #:asdf))

(defpackage :blog
  (:use :common-lisp
        :hunchentoot
        :cl-markup
        :parenscript
        :clobber
        :split-sequence)
  (:export :standard-menu))

(in-package #:herrblog.asdf)

(defsystem herrblog
  :name "blog written in common lisp"
  :version "0.0.0"
  :license "MIT"
  :author "Marek Kochanowicz (aka shka)"
  :maintainer "Marek Kochanowicz (aka shka)"
  :serial T
  :components ((:file "settings")
               (:file "generic-functions")
               (:file "common-classes")
               (:file "posts-container")
               (:file "post")
               (:file "global-definitions")
               (:file "html-templates")
               (:file "generators")
               (:file "transactions")
               (:file "handlers")))
