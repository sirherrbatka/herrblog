(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-main-page-generator* (make-instance 'main-page-generator))
(defvar *default-categories-list-generator* (make-instance 'categories-list-generator))
(defvar *default-posts-list-generator* (make-instance 'posts-list-generator))
(defvar *default-post-generator* (make-instance 'post-page-generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro page-handler (identifer &body body)
  `(slot-value (get-page ,@body
                         ,identifer)
               'm-html))


(define-easy-handler (main-page :uri "/blog") ()
  (page-handler 'main *blog*))


(define-easy-handler (categories-page :uri "/categories") ()
  (page-handler 'categories *blog*))


(define-easy-handler (blog-post :uri "/entry" :default-request-type :get) (title)
  (page-handler 'this (get-post *blog*
                                title)))


(define-easy-handler (category-page :uri "/category") (title)
  (page-handler 'posts (get-category *blog*
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
