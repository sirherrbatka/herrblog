(in-package :blog)


(defclass object-with-timestamp ()
  ((m-timestamp
    :initform (get-universal-time)
    :accessor access-timestamp)))


(defclass object-with-creation-timestamp ()
  ((m-creation-timestamp
    :accessor access-creation-timestamp
    :initarg :creation-timestamp)))


(defclass dummy-timestamp ()
  ())


(defclass cached-page (object-with-timestamp)
    ((m-cached-html
      :initarg :cached-html
      :type string
      :reader get-cached-html)))


(defun make-cached-page (html)
  (declare (type string html))
  (make-instance 'cached-page :cached-html html))


(defmethod set-cached-html ((page cached-page) value)
  (setf (slot-value page 'm-cached-html) value
        (access-timestamp page) (get-universal-time))
  page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass object-with-pages-cache (object-with-timestamp)
  ((m-cached-page-index
    :reader get-cached-page-index
    :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod created-befor ((a object-with-creation-timestamp)
                          (b object-with-creation-timestamp))
  (> (slot-value a 'm-creation-timestamp)
     (slot-value b 'm-creation-timestamp)))


(defmethod edited-before ((a object-with-timestamp)
                          (b object-with-timestamp))
  (> (slot-value a 'm-timestamp)
     (slot-value b 'm-timestamp)))


(defmethod edited-before ((a list)
                          (b list))
  (> (apply #'max (mapcar (lambda (x) (slot-value x 'm-timestamp))
                          a))
     (apply #'max (mapcar (lambda (x) (slot-value x 'm-timestamp))
                         b))))


(defmethod edited-before ((a list)
                          (b object-with-timestamp))
  (>= (apply #'max (mapcar (lambda (x) (slot-value x 'm-timestamp)) a))
      (slot-value b 'm-timestamp)))


(defmethod edited-before ((a list)
                          (b object-with-timestamp))
  (> (apply #'max (mapcar (lambda (x) (slot-value x 'm-timestamp)) a))
     (slot-value b 'm-timestamp)))


(defmethod edited-before ((a null)
                          (b object-with-timestamp))
  (error "a is null!"))


(defmethod edited-before ((a object-with-timestamp)
                          (b null))
  (error "b is null!"))


(defmethod edited-before ((a null)
                          (b null))
  (error "both a and b are null!"))


(defmethod edited-before ((a object-with-timestamp)
                         (b dummy-timestamp))
  t)


(defmethod edited-before ((a dummy-timestamp)
                         (b object-with-timestamp))
  nil)
