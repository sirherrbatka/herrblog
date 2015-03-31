(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defpost (title html-content)
  (execute 't-make-and-add-post
           title
           html-content
           (get-universal-time)))
