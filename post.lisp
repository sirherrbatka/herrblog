(in-package :blog)


(defclass post (object-with-pages-cache
                object-with-creation-timestamp
                object-with-timestamp)
  ((m-content
    :initarg :content) ;;for generating the html
   (m-title
    :initarg :title) ;;any string
   (m-id
    :initarg :id) ;;url string, created from the title
   (m-comments
    :initform nil)
   (m-tags-list
    :initarg :tags-list)))


(defclass post-comment (object-with-timestamp object-with-creation-timestamp)
  ((m-content
    :initarg :content)
   (m-author
    :initarg :author)))


(defmethod to-html ((p post))
  (apply 'markup* (cons (list :h2 (slot-value p
                                              'm-title))
                        (get-markup-from-blog-string (slot-value p
                                                                 'm-content)))))


(defmethod add-post ((blog posts-container)
                     (new-entry post))
  (with-slots ((id m-id))
      new-entry

    (with-slots ((posts m-posts)
                 (post-ids m-post-ids)
                 (post-count m-post-count)
                 (timestamp m-timestamp))
        blog

      (if (null (gethash id
                         posts))

          (progn
            (incf post-count)
            (setf (gethash id
                           posts)
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


(defun make-post (title html-content time)
  (declare (type string title)
           (type string html-content)) ;;temporary
  (make-instance 'post
                 :title title
                 :tags-list nil
                 :id (id-from-title title)
                 :creation-timestamp time
                 :content html-content
                 :generators (list (list 'this
                                         'generate-post-page
                                         (lambda (x) x)))))
