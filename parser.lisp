(in-package :blog)


(defvar *markup-definitions* (make-hash-table :test 'equal))


(defun add-to-definitions (key value)
  (setf (gethash key *markup-definitions*)
        value))


(add-to-definitions "!par" :P)

(add-to-definitions "!end" :End)


(defun group (lst)
  (declare (type list lst))
  (labels ((find-end (list)
             (if (eq :End (car list))
                 (cdr list)
                 (find-end (cdr list))))

           (worker (ac next)
             (cond ((or (endp next)
                        (eq (car next) :End))
                    (reverse ac))

                   ((symbolp (car next))
                    (worker (cons (worker (list (car next))
                                          (cdr next))
                                  ac)
                            (find-end (cdr next))))

                   (t
                    (worker (cons (car next)
                                  ac)
                            (cdr next))))))

    (worker nil lst)))


(defun to-markup-list (lst)
  (declare (type list lst))
  (nreverse (reduce (lambda (prev next)
                      (cond ((some 'symbolp
                                   (list (car prev) next))
                             (cons next prev))

                            ((stringp (car prev))
                             (cons (stringify (car prev)
                                              " "
                                              next)
                                   (cdr prev)))))

                    (mapcar (lambda (x)
                              (let ((tag (gethash x
                                                  *markup-definitions*)))
                                (if (null tag)
                                    x
                                    tag)))
                            lst)
                    :initial-value nil)))


(defun get-markup-from-blog-string (str)
  (declare (type string str))
  (group (to-markup-list (split "\\s+"
                                str))))
