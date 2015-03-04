(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-class-with-initializer (class-name
                                         direct-superclasses
                                         direct-slots
                                         options
                                         initialization-function
                                         argument-name)
  (let* ((slots-to-initialize (mapcar 'car (remove-if-not (lambda (x) (null (find :initform x))) direct-slots))))
    (flet ((make-setf-form (x)
             (list 'setf (list 'slot-value argument-name (list 'quote x))
                   x)))
      `(progn (defun ,initialization-function ,(cons argument-name slots-to-initialize)
                (declare (type ,class-name ,argument-name))
                (progn
                  ,(reduce 'append (mapcar #'make-setf-form slots-to-initialize))
                  ,argument-name))
              ,(append `(defclass ,class-name ,direct-superclasses ,direct-slots) options)))))


(defmacro bind-defun (name binding-function &body arguments)
  `(defun ,name ()
     (,binding-function ,@arguments)))


(defmacro define-init-chain (name argument-name &body initialization-functions)
  (flet ((combine (fn)
           (list fn argument-name)))
    `(defun ,name (,argument-name)
       ,(cons 'progn (mapcar #'combine initialization-functions)))))
