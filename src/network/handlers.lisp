(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-main-page-generator* (init-default-whole-main-page-generator (make-instance 'main-page-generator)))
(defvar *default-categories-list-generator* (init-default-whole-categories-list-generator (make-instance 'categories-list-generator)))
(defvar *default-posts-list-generator* (init-default-whole-posts-list-generator (make-instance 'posts-list-generator)))
(defvar *default-post-generator* (init-default-whole-post-page-generator (make-instance 'post-page-generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-easy-handler (main-page :uri "/blog") ()
  (generate-page-from *default-main-page-generator*
                      *blog*)) ;;TODO: handle exception


(define-easy-handler (categories-page :uri "/categories") ()
  (generate-page-from *default-categories-list-generator*
                      *blog*)) ;;TODO: handle exception


(define-easy-handler (blog-post :uri "/entry" :default-request-type :get) (title)
  (generate-page-from *default-post-generator*
                      (get-post *blog* title))) ;;TODO: handle exception


(define-easy-handler (category-page :uri "/category") (title)
  (generate-page-from *default-posts-list-generator*
                      (get-category *blog*
                                    title))) ;;TODO: Handle exception


(define-easy-handler (posts-page :uri "/posts") ()
  (generate-page-from *default-posts-list-generator*
                      *blog*)) ;;TODO: handle exception

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
