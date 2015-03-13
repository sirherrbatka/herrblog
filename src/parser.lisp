(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun key-in-hash (sym expansion-map)
  (nth-value 1 (gethash sym expansion-map)))


(defun expand (sym body map)
  (let ((expander (gethash sym map)))
    (the list (funcall expander body))))


(defun expand-tree (tree expansion-map)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *markup-definitions* (make-hash-table :test 'equal))
(defvar *external-level-symbols* #(:P :End))
(defvar *internal-level-symbols* #(:list-element-termination))

(defmacro define-symbol-category (symbols-sequence typename predictate)
  (let ((argument (gensym)))
        `(defun ,typename (,argument)
           (declare (type symbol ,argument))
           (not (null (find ,argument ,symbols-sequence))))
        `(deftype ,typename ()
           '(satisfies ,typename))))


(define-symbol-category *external-level-symbols* external-level-symbol external-level-symbolp)
(define-symbol-category *internal-level-symbols* internal-level-symbol internal-level-symbolp)


(defun add-to-definitions (key value)
  (setf (gethash key *markup-definitions*)
        value))


(add-to-definitions "!par" :P)
(add-to-definitions "!end" :End)


(defun group (lst)
  (declare (type list lst))
  (labels ((find-end (list)
             (if (or (eq :End (car list))
                     (endp (car list)))
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
                      (cond ((some 'symbolp (list (car prev) next))
                             (cons next prev))

                            ((stringp (car prev))
                             (cons (stringify (car prev)
                                              " "
                                              next)
                                   (cdr prev)))))

                    (mapcar (lambda (x)
                              (let ((tag (gethash x *markup-definitions*)))
                                (if (null tag)
                                    x
                                    tag)))
                            lst)
                    :initial-value nil)))


(defun get-markup-from-blog-string (str)
  (declare (type string str))
  (group (to-markup-list (split "\\s+" str))))
