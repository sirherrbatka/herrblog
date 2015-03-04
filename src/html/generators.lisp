(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass caching-generator ()
  ((m-cached-html
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :accessor access-cached-html)))


(defmethod reset-cache ((object caching-generator))
  (setf (access-cached-html object)
        (make-hash-table :test #'equal)))


(defmacro define-reseting-accessor (accessor slot)
  `(defmethod (setf ,accessor) :after (new-value (object caching-generator))
              (unless (= (slot-value object ,slot) new-value)
                (reset-cache object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass styled-page-generator ()
  ((m-style-html
    :type string
    :initarg :style-html
    :accessor access-style-html)
   (m-style-body
    :type string
    :initarg :style-body
    :accessor access-style-body)
   (m-style-body-after
    :type string
    :initarg :style-body-after
    :accessor access-style-body-after)
   (m-style-header
    :type string
    :initarg :style-body
    :accessor access-style-header)
   (m-style-h1
    :type string
    :initarg :style-h1
    :accessor access-style-h1)
   (m-style-menu
    :type string
    :initarg :style-menu
    :accessor access-style-menu)
   (m-style-article
    :type string
    :initarg :style-article
    :accessor access-style-article)
   (m-style-article-footer
    :type string
    :initarg :style-article-footer
    :accessor access-style-article-footer)
   (m-style-display
    :type string
    :initarg :style-display
    :accessor access-style-display)
   (m-style-line
    :type string
    :initarg :style-line
    :accessor access-style-line)))


(define-reseting-accessor access-style-html m-style-html)
(define-reseting-accessor access-style-body m-style-body)
(define-reseting-accessor access-style-body-after m-style-body-after)
(define-reseting-accessor access-style-header m-style-header)
(define-reseting-accessor access-style-h1 m-style-h1)
(define-reseting-accessor access-style-menu m-style-menu)
(define-reseting-accessor access-style-article m-style-article)
(define-reseting-accessor access-style-article-footer m-style-article-footer)
(define-reseting-accessor access-style-display m-style-display)
(define-reseting-accessor access-style-line m-style-line)


(defmethod get-style stringify ((generator styled-page-generator))
  (stringify (access-style-html object)
             (access-style-body object)
             (access-style-body-after object)
             (access-style-header object)
             (access-style-h1 object)
             (access-style-menu object)
             (access-style-article object)
             (access-style-article-footer object)
             (access-style-display object)
             (access-style-line object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass no-columns-page-generator ()
  ((m-style-main
    :type string
    :initform  "main { width: 73%; padding: 1%; float: right; }"
    :accessor access-style-main)))


(defmethod get-style stringify ((generator simple-page-generator))
  (access-style-main generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass columns-page-generator ()
  ((m-columns-count
    :type (unsigned-byte 8)
    :initarg :columns-count
    :accessor access-columns-count)
   (m-style-columns-main
    :type string
    :accessor access-style-columns-main)
   (m-style-columns-main-template
    :type string
    :initform "main { width: 73%; padding: 1%; float: right; webkit-columns: $$; -moz-columns: $$; columns: $$; }"
    :allocation :class)))


(define-reseting-accessor access-columns-count m-columns-count)


(defun update-style-columns-main (generator)
  (declare (type columns-page-generator generator))
  (setf (slot-value generator 'm-style-columns-main)
        )) ;;TODO generate new string by substituting the $$ for printed representation of the new-value


(defmethod initialize-instance :after ((object columns-page-generator) &key columns-count)
  (update-style-columns-main object))


(defmethod (setf access-columns-count) :after (new-value (object columns-page-generator))
  (update-style-columns-main object))


(defmethod get-style stringify ((generator columns-page-generator))
  (access-style-columns-main generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass main-page-generator (caching-generator styled-page-generator no-columns-page-generator)
  ((m-posts-on-main-page-count
    :type (unsigned-byte 8)
    :initarg :posts-on-main-page-count
    :accessor access-posts-on-main-page-count)))


(define-reseting-accessor access-posts-on-main-page-count m-posts-on-main-page-count)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass posts-list-generator (caching-generator columns-page-generator styled-page-generator)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass categories-list-generator (caching-generator columns-page-generator styled-page-generator)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass post-page-generator (caching-generator styled-page-generator no-columns-page-generator)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod generate-page-from :start ((generator caching-generator)
                                      (object object-with-pages-cache))
  (with-accessors ((m-cached-html access-cached-html)) generator
    (multiple-value-bind (cached founded) (gethash (access-cached-page-index object) access-cached-html)
      (get-cached-html (if (or (not founded)
                               (edited-before object cached))
                           (setf (gethash (access-cached-page-index object) access-cached-html)
                                 (make-cached-page (call-next-method)))
                           cached)))))


(defmethod generate-page-from ((generator post-page-generator)
                               (object post))
  (standard-page
   (get-style generator)
   (get-menu)
   (access-title object)
   (to-html object)
   (markup* '(:hr))))


(defmethod generate-page-from ((generator main-page-generator)
                               (object main-container))
  (flet ((generate-posts-html (posts-list)
           (reduce #'stringify
                   (mapcar (lambda (x) (stringify (to-html x)
                                                  (markup* (list :a :href (format nil
                                                                                  "entry?title=~a"
                                                                                  (slot-value x
                                                                                              'm-id))
                                                                 "Comments"))))
                          posts-list))))

    (markup* (list :html
                   (standard-page
                    (get-style generator)
                    (get-menu (list "categories" "Categories"))
                    "Main Page"
                    (generate-posts-html (get-most-recent-posts
                                          object
                                          *posts-on-main-page*)))))))


(defmethod generate-page-from ((generator posts-list-generator)
                               (object posts-container))
  (markup* (list :html
                 (standard-page
                  (get-style generator)
                  (get-menu)
                  "Posts"
                  (reduce #'stringify (mapcar (lambda (x) (markup* (list :li (list :a :href (format nil
                                                                                                    "entry?title=~a"
                                                                                                    (slot-value x
                                                                                                                'm-id)))
                                                                          (slot-value x
                                                                                      'm-title))))

                                              (mapcar (lambda (x) (get-post object x))
                                                       (slot-value object 'm-post-ids))))))))


(defmethod generate-page-from ((generator categories-list-generator)
                               (object main-container))
  (markup* (list :html
                 (standard-page
                  (get-style generator)
                  (get-menu)
                  "Categories"
                  (reduce #'stringify (mapcar (lambda (x) (markup* (list :li (list :a :href (format nil
                                                                                                    "category?title=~a"
                                                                                                    x))
                                                                         (slot-value x
                                                                                     'm-title))))

                                              (hash-keys (slot-value object
                                                                     'm-categories))))))))


(defun generate-new-post-page ()
  (with-http-authentication
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
                       (:p (:input :type "submit" :value "Add" :class "btn")))))))


(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))
