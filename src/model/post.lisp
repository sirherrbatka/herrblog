(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass post (object-with-creation-timestamp
                object-with-pages-cache)
  ((m-content
    :initarg :content
    :accessor access-content
    :type list) ;;for generating the html
   (m-title
    :initarg :title
    :type string
    :accessor access-title) ;;any string
   (m-id
    :type string
    :initarg :id) ;;url string, created from the title
   (m-comments
    :initform nil)
   (m-tags-list ;;AKA categories
    :accessor access-tags-list
    :type list
    :initarg :tags-list)
   (m-preceding
    :accessor access-preceding)
   (m-following
    :accessor access-following)))

(defmethod initialize-instance ((object post) &key title id tags-list)
  (setf (slot-value object 'm-title) title
        (slot-value object 'm-id) id
        (slot-value object 'm-tags-list) tags-list)
  (call-next-method))

(defclass post-comment (object-with-timestamp
                        object-with-creation-timestamp)
  ((m-content
    :initarg :content
    :accessor access-content
    :type string)
   (m-author
    :initarg :author
    :accessor access-author
    :type string)))


(defmethod to-html ((p post)
                    (rules hash-table))
  (apply #'markup* (cons (list :h2 (access-title p))
                         (expand-tree (access-content p)
                                      rules))))


(defmethod add-post ((blog main-container)
                     (new-entry post))
  (with-slots ((id m-id)
               (tags-list m-tags-list)) new-entry

    (with-slots ((posts m-posts)
                 (post-ids m-post-ids)
                 (post-count m-post-count)
                 (timestamp m-timestamp)
                 (categories m-categories)) blog

      (mapc (lambda (x) (multiple-value-bind (cat found) (gethash x categories)
                          (add-post (if found
                                        cat
                                        (new-category (string-to-id x) blog))
                                    new-entry)))
            tags-list)))

  (call-next-method))


(defmethod remove-post ((container main-container)
                        (id string))
  (with-slots ((categories m-categories)
               (posts m-posts))
      (multiple-value-bind (post found) (gethash id posts)
        (if found
            (progn
              (mapcan (lambda (x) (remove-post x id))
                      (mapcar (lambda (y) (gethash y categories))
                              (slot-value post 'm-tags-list)))
              (call-next-method))
            (error "No such post")))))


(defmethod remove-post ((container posts-container)
                        (id string))
  (with-slots ((posts m-posts)
               (post-ids m-post-ids)
               (post-count m-post-count)
               (timestamp m-timestamp))
      (multiple-value-bind (post found) (gethash id posts)
        (if found
            (progn
              (decf post-count)
              (delete id post-ids :test :equal)
              (remhash id posts)
              (setf timestamp (get-universal-time)))
            (error "No such post")))))


(defmethod add-post ((blog posts-container)
                     (new-entry post))
  (with-slots ((id m-id))
      new-entry

    (with-slots ((posts m-posts)
                 (post-ids m-post-ids)
                 (post-count m-post-count)
                 (timestamp m-timestamp))
        blog
      (if (null (gethash id posts))
          (progn
            (incf post-count)
            (setf (gethash id posts)
                  new-entry)
            (push id
                  post-ids)
            (setf timestamp
                  (get-universal-time)))
          (error "Entry already present")))))


(defun make-post (title html-content time &optional (tags nil))
  (declare (type string title)
           (type list html-content)
           (type list tags))
  (let ((id (string-to-id title)))
    (make-instance 'post
                   :title title
                   :tags-list tags
                   :cached-page-index id
                   :id id
                   :creation-timestamp time
                   :content html-content)))
