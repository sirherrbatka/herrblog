(in-package :blog)


(defun hash-keys (hash-table)
  (declare (type hash-table hash-table))
  (loop for key being the hash-keys of hash-table collect key))


(defun stringify (&rest rest)
  (if (endp rest)
      ""
      (reduce (lambda (a b) (concatenate 'string a b))
              rest)))
