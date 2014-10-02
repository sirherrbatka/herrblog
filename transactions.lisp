(in-package :blog)


(defun t-make-and-add-post (title html-content time)
  (add-post *blog*
            (make-post title
                       html-content
                       time)))


(defun t-remove-post (sym)
  (declare (type symbol sym))
  (with-slots ((post-ids m-post-ids)
               (post-count m-post-count)
               (posts m-posts))
      *blog*
    (remhash sym
             posts)
    (decf post-count 1)
    (setf post-ids
          (delete sym
                  post-ids
                  :test #'eq))))


(defun t-add-comment (entry author content time)
  (let ((target (gethash entry
                         (slot-value *blog*
                                     'm-posts))))
    (if (null target)
        (error "No such post")
        (with-slots ((timestamp m-timestamp)
                     (comments m-comments))
            target
          (setf comments (nreverse (cons (make-instance 'entry-comment
                                                        :content content
                                                        :author author
                                                        :creation-timestamp time)
                                         comments)))
          (setf timestamp (get-universal-time))))))
