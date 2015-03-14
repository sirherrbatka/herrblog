(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun key-in-hash (sym expansion-map)
  (declare (type symbol sym)
           (type hash-table expansion-map))
  (the symbol (nth-value 1 (gethash sym expansion-map))))


(defun expand (sym body map)
  (declare (type symbol sym)
           (type list body)
           (type hash-table map))
  (let ((expander (gethash sym map)))
    (funcall expander body)))


(defun expand-tree (tree expansion-map)
  (declare (type list tree)
           (type hash-table expansion-map))
  (labels ((worker (input ac)
             (let ((first (car input))
                   (rest (cdr input)))
               (cond
                 ((null first)
                  (reverse ac))
                 ((atom first)
                  (worker rest (cons first ac)))
                 ((key-in-hash (car first) expansion-map)
                  (worker rest
                          (cons (expand (car first)
                                        (cdr first)
                                        expansion-map)
                                ac)))
                 (t
                  (worker rest
                          (cons (worker first nil)
                                ac)))))))
    (worker tree nil)))


(defun make-expand-map (&rest functions)
  (let ((product (make-hash-table :test 'eq)))
    (mapc (lambda (x) (setf (gethash (car x) product)
                            (cadr x)))
          functions)
    product))


(defvar *default-expansion-map* (make-expand-map (list :paragraph (lambda (x) (cons :p x)))
                                                 (list :points (lambda (x) (list :ui (mapcar (lambda (y) (list :li y))
                                                                                             x))))))
