(in-package :blog)


(defun stringify (&rest rest)
  (if (endp rest)
      ""
      (reduce (lambda (a b) (concatenate 'string a b))
              rest)))
