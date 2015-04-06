(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-class-with-initializer (class-name
                                         direct-superclasses
                                         direct-slots
                                         options
                                         initialization-function
                                         argument-name)
  (let* ((slots-to-initialize (mapcar 'car (remove-if-not (lambda (x) (null (find :initform x))) direct-slots))))
    (flet ((make-setf-form (x)
             `(unless (slot-boundp ,argument-name (quote ,x))
                (setf (slot-value ,argument-name (quote ,x))
                        ,x))))
      `(progn ,(append `(defclass ,class-name ,direct-superclasses ,direct-slots) options)
              (defun ,initialization-function ,(cons argument-name slots-to-initialize)
                (declare (type ,class-name ,argument-name))
                (progn
                  ,(cons 'progn (mapcar #'make-setf-form slots-to-initialize))
                  ,argument-name))))))


(defmacro bind-defun (name binding-function free-arguments &body arguments)
  `(defun ,name ,free-arguments
     (,binding-function ,@free-arguments ,@arguments)))


(defmacro define-init-chain (name argument-name &body initialization-functions)
  (flet ((combine (fn)
           (list fn argument-name)))
    `(defun ,name (,argument-name)
       ,(cons 'progn (mapcar #'combine initialization-functions)))))
