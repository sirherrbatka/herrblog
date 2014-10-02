(in-package :blog)


(defun substitute-with-markup (string-list)
  )

(defun get-html (s)
  (declare (type string s))
  (markup* (merge-strings (substitute-with-markup (split-sequence #\space
                                                                  s)))))
