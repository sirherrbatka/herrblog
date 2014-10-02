(in-package :blog)


(defclass posts-container (object-with-timestamp object-with-pages-cache)
  ((m-posts
    :initarg :posts)
   (m-post-ids
    :initarg :post-ids)
   (m-post-count
    :initform 0)))


(defmethod get-post ((blog posts-container) (id string))
  (let ((out (gethash id
                      (slot-value blog
                                  'm-posts))))

    (if (null out)
        (error "No post with such id: ~S" id)
        out)))


(defmethod get-most-recent-posts ((blog posts-container) (count integer))
  (mapcar (lambda (id) (get-post blog
                                 id))
          (subseq (slot-value blog
                              'm-post-ids)
                  0
                  (min count
                       (slot-value blog
                                   'm-post-count)))))


(defmethod sort-content ((container posts-container))
  (with-slots ((post-ids m-post-ids)
               (posts m-posts))
      container

    (setf post-ids (sort post-ids
                         (lambda (a b) (> (slot-value (get-post container a)
                                                      'm-creation-timestamp)
                                          (slot-value (get-post container b)
                                                      'm-creation-timestamp)))))
    container))
