(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (with-slots ((categories m-categories)) blog
    (let ((found (nth-value 1 (gethash id categories))))
      (if found
          (error "Category already exists!")
          (prog1
              (setf (gethash id categories)
                    (make-instance 'posts-category
                                   :category-name id
                                   :cached-page-index id))
            (update-timestamp blog))))))


(defmethod get-category ((blog posts-container) id)
  (declare (type string id))
  (let ((out (gethash id (slot-value blog 'm-categories))))
    (if (null out)
        (error 'page-not-found "No category with such id: ~S" id)
        out)))


(defmethod get-post ((blog posts-container) id)
  (declare (type string id))
  (let ((out (gethash id
                      (slot-value blog
                                  'm-posts))))

    (if (null out)
        (error 'page-not-found "No post with such id: ~S" id)
        out)))


(defmethod get-most-recent-posts ((blog posts-container) count)
  (declare (type integer count))
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
