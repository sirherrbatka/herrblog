(in-package :blog)


(defclass posts-container (object-with-pages-cache)
  ((m-posts
    :initform (make-hash-table :test 'equal))
   (m-post-ids
    :initform nil
    :reader get-post-ids)
   (m-post-count
    :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass posts-category (posts-container)
  ((m-category-name
    :initarg :category-name)))


(defmethod initialize-instance :after ((c posts-category) &key)
  (setf (slot-value c 'm-cached-page-index)
        (slot-value c 'm-category-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass main-container (posts-container)
  ((m-categories
    :initform (make-hash-table :test 'equal)
    :accessor access-categories)))


(defmethod initialize-instance :after ((m main-container) &key)
  (setf (slot-value m 'm-cached-page-index)
        'blog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod new-category ((id string)
                         (blog main-container))
  (multiple-value-bind (category found) (gethash id (slot-value blog 'm-categories))
    (if found
        (error "Category already exists!")
        (setf (gethash id (slot-value blog 'm-categories))
              (make-instance 'posts-category
                             :category-name id
                             :generator (list (list 'posts
                                                    'generate-posts-page
                                                    (lambda (x) x))))))))


(define-condition no-such-page (error) ())


(defmethod get-category ((blog posts-container) (id string))
  (let ((out (gethash id
                      (slot-value blog
                                  'm-categories))))

    (if (null out)
        (error 'no-such-page "No category with such id: ~S" id)
        out)))


(defmethod get-post ((blog posts-container) (id string))
  (let ((out (gethash id
                      (slot-value blog
                                  'm-posts))))

    (if (null out)
        (error 'no-such-page "No post with such id: ~S" id)
        out)))


(defmethod get-most-recent-posts ((blog posts-container) (count integer))
  (mapcar (lambda (id) (get-post blog id))
          (subseq (slot-value blog
                              'm-post-ids)
                  0
                  (min count
                       (slot-value blog
                                   'm-post-count)))))


(defmethod sort-content ((container posts-container))
  (with-slots ((post-ids m-post-ids) (posts m-posts))
      container

    (setf post-ids (sort post-ids
                         (lambda (a b) (> (slot-value (get-post container a)
                                                      'm-creation-timestamp)
                                          (slot-value (get-post container b)
                                                      'm-creation-timestamp)))))
    container))
