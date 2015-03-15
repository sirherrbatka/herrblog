(in-package :blog)

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
    :initarg :tags-list)))


(defmethod initialize-instance :after ((post post) &key)
  (setf (access-cached-page-index post)
        (slot-value post 'm-id)))


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
  (apply 'markup* (cons (list :h2 (access-title p))
                        (expand-tree (access-content p)
                                     rules))))


(defmethod add-post ((blog main-container)
                     (new-entry post))
  (with-slots ((id m-id)
               (tags-list m-tags-list))
      new-entry

    (with-slots ((posts m-posts)
                 (post-ids m-post-ids)
                 (post-count m-post-count)
                 (timestamp m-timestamp)
                 (categories m-categories))
        blog

      (mapc (lambda (x) (multiple-value-bind (cat found) (gethash x categories)
                          (if found
                              (add-post cat new-entry)
                              (add-post (new-category x blog) new-entry))))
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


(defun title-string-to-url-string (title)
  (declare (type string title))
  (string-downcase (string-trim " "
                                (substitute #\-
                                            #\Space
                                            title))))


(defun id-from-title (str)
  (declare (type string str))
  (string-upcase (title-string-to-url-string str)))


(defun url-from-id (symb)
  (declare (type symbol symb))
  (string-downcase (string symb)))


(defun id-from-url (url)
  (declare (type string url))
  (string-upcase url))


(defun make-post (title html-content time &optional (tags nil))
  (declare (type string title html-content)
           (type list tags)) ;;temporary
  (let ((id (id-from-title title)))
    (make-instance 'post
                   :title title
                   :tags-list tags
                   :id id
                   :cached-page-index id
                   :creation-timestamp time
                   :content html-content)))
