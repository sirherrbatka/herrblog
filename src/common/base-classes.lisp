(in-package :blog)


(defclass object-with-timestamp ()
  ((m-timestamp
    :initform (get-universal-time)
    :accessor access-timestamp))
  :documentation "timestamp is supposed to be updated after data modification")


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
      :reader get-cached-html))
  :documentation "Holds cached html as string inside the m-cached-html.")


(defun make-cached-page (html)
  (declare (type string html))
  (make-instance 'cached-page :cached-html html))


(defmethod set-cached-html ((page cached-page) value)
  (setf (slot-value page 'm-cached-html) value
        (access-timestamp page) (get-universal-time))
  page)


(defclass object-with-pages-cache (object-with-timestamp)
  ((m-cached-page-index
    :initarg :cached-page-index
    :accessor access-cached-page-index))
  :documentation "Holds symbol to identiy cached html inside the cached generator instance.")


(defmethod initialize-instance ((s object-with-pages-cache) &key generators)
  (with-slots ((table m-cache-with-generators))
      s
    (setf table
          (make-hash-table))
    (mapc (lambda (x) (setf (gethash (car x)
                                     table)
                            (make-instance 'page-composite
                                           :generator (second x)
                                           :selector (third x))))
          generators))
  (call-next-method))


(defmethod get-page ((object object-with-pages-cache) (identifer symbol))
  (with-slots ((table m-cache-with-generators))
      object
    (let ((composite (gethash identifer
                              table)))

      (when (null composite)
        (error "Identifer is not known in this object"))

      (if (or (null (slot-value composite
                                'm-cached-page))
              (edited-before (funcall (slot-value composite
                                                  'm-selector)
                                      object)
                             (slot-value composite
                                         'm-cached-page)))

          (setf (slot-value composite
                            'm-cached-page)
                (funcall (slot-value composite
                                     'm-generator)

                         object))

          (slot-value composite
                      'm-cached-page)))))





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
