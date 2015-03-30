(in-package :blog)


(defun t-make-and-add-post (title html-content time)
  (declare (type string title)
           (type list html-content))
  (add-post *blog*
            (make-post title
                       html-content
                       time)))


(defun t-remove-post (id)
  (declare (type string id))
  (remove-post *blog*
               id))


(defun t-add-comment (entry author content time)
  (let ((target (gethash entry
                         (slot-value *blog*
                                     'm-posts))))
    (if (null target)
        (error "No such post")
        (with-slots ((timestamp m-timestamp)
                     (comments m-comments))
            target
          (setf comments (nreverse (cons (make-instance 'post-comment
                                                        :content content
                                                        :author author
                                                        :creation-timestamp time)
                                         comments)))
          (setf timestamp (get-universal-time))))))
