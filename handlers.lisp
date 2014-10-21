(in-package :blog)


(defmacro page-handler (identifer &body body)
  `(slot-value (get-page ,@body
                         ,identifer)
               'm-html))


(define-easy-handler (main-page :uri "/blog") ()
  (page-handler 'main *blog*))


(define-easy-handler (categories-page :uri "/categories") ()
  (page-handler 'categories *blog*))


(define-easy-handler (blog-page :uri "/entry") (title)
  (page-handler 'this (get-post *blog*
                                title)))


(define-easy-handler (posts-page :uri "/posts") ()
  (page-handler 'posts *blog*))


(define-easy-handler (adding-new-post :uri "/new-post") ()
  (generate-new-post-page))


(define-easy-handler (new-post-added :uri "/post-added") (title content)
  (execute 't-make-and-add-post
           title
           content
           (get-universal-time))
  (redirect "/blog"))
