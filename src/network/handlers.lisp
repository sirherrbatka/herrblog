(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-main-page-generator* (init-default-whole-main-page-generator (make-instance 'main-page-generator)))
(defvar *default-categories-list-generator* (init-default-whole-categories-list-generator (make-instance 'categories-list-generator)))
(defvar *default-posts-list-generator* (init-default-whole-posts-list-generator (make-instance 'posts-list-generator)))
(defvar *default-post-generator* (init-default-whole-post-page-generator (make-instance 'post-page-generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-with-page-not-found (&body body)
  `(handler-case
       (progn ,@body)
     (page-not-found (e)
       "Page not found")))


(define-easy-handler (main-page :uri "/blog") ()
  (define-with-page-not-found
      (generate-page-from *default-main-page-generator*
                          *blog*)))


(define-easy-handler (categories-page :uri "/categories") ()
  (define-with-page-not-found
      (generate-page-from *default-categories-list-generator*
                          *blog*))) ;;TODO: handle exception


(define-easy-handler (blog-post :uri "/entry" :default-request-type :get) (title)
  (define-with-page-not-found
      (generate-page-from *default-post-generator*
                          (get-post *blog* title)))) ;;TODO: handle exception


(define-easy-handler (category-page :uri "/category") (title)
  (define-with-page-not-found
      (generate-page-from *default-posts-list-generator*
                          (get-category *blog*
                                        title)))) ;;TODO: Handle exception


(define-easy-handler (posts-page :uri "/posts") ()
  (define-with-page-not-found
      (generate-page-from *default-posts-list-generator*
                          *blog*))) ;;TODO: handle exception

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
