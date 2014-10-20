(in-package :blog)


(defmethod generate-post-page ((p post))
  (make-instance 'cached-page
                 :html (standard-page
                           (get-style)
                           (get-menu)
                           (slot-value p
                                       'm-title)
                         (to-html p))))


(defmethod generate-main-page ((blog posts-container))
  (flet ((generate-posts-html (posts-list)
           (apply 'stringify
                  (mapcar (lambda (x) (stringify (to-html x)
                                                 (markup* (list :a :href (format nil
                                                                                 "entry?title=~a"
                                                                                 (slot-value x
                                                                                             'm-id))
                                                                "Comments"))))
                          posts-list))))

    (make-instance 'cached-page
                   :html (standard-page
                             (get-style)
                             (get-menu (list "" "Categories"))
                             "Main Page"
                           (generate-posts-html (get-most-recent-posts
                                                 blog
                                                 *posts-on-main-page*))))))


(defmethod generate-posts-page ((blog posts-container))
  (make-instance 'cached-page
                 :html (standard-page
                           (get-style :columns-for-main t)
                           (get-menu)
                           "Posts"
                         (apply 'stringify (mapcar (lambda (x) (markup* (list :li (list :a :href (format nil
                                                                                                         "entry?title=~a"
                                                                                                         (slot-value x
                                                                                                                     'm-id)))
                                                                              (slot-value x
                                                                                          'm-title))))

                                                   (mapcar (lambda (x) (get-post blog
                                                                                 x))
                                                           (slot-value blog
                                                                       'm-post-ids)))))))


(defun generate-new-post-page ()
  (standard-page
      (get-style)
      (get-menu)
      "New Post"
      (markup (:h2 "Add a new post")
              (:form :action "/post-added" :method "post" :id "addform"
                     (:p "Title" (:br)
                         (:input :type "text" :name "title" :class "txt"))
                     (:p "Content" (:br)
                         (:textarea :name "content" :cols 80 :rows 20)
                         (:/textarea))
                     (:p (:input :type "submit" :value "Add" :class "btn"))))))
