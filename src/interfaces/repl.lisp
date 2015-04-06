(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defpost (title html-content tags)
  (declare (type string title)
           (type list html-content tags))
  (execute 't-make-and-add-post
           title
           html-content
           (get-universal-time)
           tags))
