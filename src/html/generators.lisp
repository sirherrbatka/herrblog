(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass caching-generator ()
  ((m-cached-html
    :initform (make-hash-table :test #'equal)
    :type hash-table
    :accessor access-cached-html)))


(defmethod reset-cache ((object caching-generator))
  (setf (access-cached-html object)
        (make-hash-table :test #'equal)))


(defmacro define-reseting-accessor (accessor slot)
  `(defmethod (setf ,accessor) :before (new-value (object caching-generator))
              (when (and (slot-boundp  object (quote ,slot))
                         (not (equal (slot-value object (quote ,slot))
                                     new-value)))
                (reset-cache object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer styled-page-generator ()
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
    :accessor access-style-line)
   (m-style-link
    :type string
    :initarg :style-link
    :accessor access-style-link)
   (m-style-code
    :type string
    :initarg :style-code
    :accessor access-style-code))
  nil
  init-styled-page-generator styled-page-generator)


(bind-defun
    init-default-styled-page-generator
    init-styled-page-generator
    (styled-page-generator)
  *default-html*
  *default-body*
  *default-body-after*
  *default-header*
  *default-h1*
  *default-article*
  *default-article-footer*
  *default-display*
  *default-line*
  *default-link*
  *default-source*)


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
(define-reseting-accessor access-style-link m-style-link)
(define-reseting-accessor access-style-code m-style-code)


(defmethod get-style stringify ((generator styled-page-generator))
           (stringify (access-style-html generator)
                      (access-style-body generator)
                      (access-style-body-after generator)
                      (access-style-header generator)
                      (access-style-h1 generator)
                      (access-style-menu generator)
                      (access-style-article generator)
                      (access-style-article-footer generator)
                      (access-style-display generator)
                      (access-style-line generator)
                      (access-style-link generator)
                      (access-style-code generator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer with-menu-page-generator ()
  ((m-style-menu
    :type string
    :initarg :style-menu
    :accessor access-style-menu)
   (m-additional-menu-items
    :type list
    :initarg :additional-menu-items
    :accessor access-additional-menu-items)
   (m-main-menu-items
    :type list
    :initarg :main-menu-items
    :accessor access-main-menu-items))
  nil
  init-with-menu-page-generator with-menu-page-generator)


(bind-defun
    init-default-with-menu-page-generator
    init-with-menu-page-generator
    (with-menu-page-generator)
            *default-menu*
            nil
            *default-menu-items*)


(define-reseting-accessor access-style-menu m-style-menu)
(define-reseting-accessor access-additional-menu-items m-additional-menu-items)
(define-reseting-accessor access-main-menu-items m-main-menu-items)


(defmethod get-style stringify ((generator with-menu-page-generator))
  (access-style-menu generator))


(defmethod get-menu ((generator with-menu-page-generator)
                     (object T))
  (markup* (apply #'standard-menu (compose-menu generator object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass no-columns-page-generator ()
  ((m-style-main
    :type string
    :initform  "main { width: 73%; padding: 1%; float: right; }"
    :accessor access-style-main)))


(defmethod get-style stringify ((generator no-columns-page-generator))
  (access-style-main generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer columns-page-generator ()
  ((m-columns-count
    :type (unsigned-byte 8)
    :initarg :columns-count
    :accessor access-columns-count)
   (m-style-columns-main
    :type string
    :accessor access-style-columns-main)
   (m-style-columns-main-template
    :type string
    :allocation :class))
  nil
  init-columns-page-generator columns-page-generator)


(define-reseting-accessor access-style-columns-main m-style-columns-main)


(defun update-style-columns-main (generator)
  (declare (type columns-page-generator generator))
  (with-slots ((style-columns-main-template m-style-columns-main-template)
               (columns-count m-columns-count)
               (style-columns-main m-style-columns-main)) generator
      (setf style-columns-main
            (regex-replace-all "\@"
                               style-columns-main-template
                               (write-to-string columns-count)))
      (reset-cache generator)))


(defmethod initialize-instance :around ((object columns-page-generator) &key (columns-count 3))
  (setf (slot-value object 'm-style-columns-main-template) "main { width: 73%; padding: 1%; float: right; webkit-columns: @; -moz-columns: @; columns: @; }"
        (slot-value object 'm-columns-count) columns-count)
  (update-style-columns-main object)
  (call-next-method))


(defmethod (setf access-columns-count) :after (new-value (object columns-page-generator))
  (update-style-columns-main object))


(defmethod get-style stringify ((generator columns-page-generator))
  (access-style-columns-main generator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass common-generator (caching-generator styled-page-generator with-menu-page-generator)
  ())


(define-init-chain init-default-common-generator common-generator
  init-default-styled-page-generator
  init-default-with-menu-page-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass comments-page-generator (no-columns-page-generator
                                   styled-page-generator
                                   with-menu-page-generator)
  ())


(define-init-chain init-default-comments-page-generator comments-page-generator
  init-default-styled-page-generator
  init-default-with-menu-page-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer with-post-page-generator ()
  ((m-expansion-map
    :accessor access-expansion-map)
   (m-post-style
    :accessor access-post-style))
  nil
  init-with-post-page-generator
  post-page-generator)


(defmethod get-style stringify ((generator with-post-page-generator))
  (access-post-style generator))


(define-reseting-accessor access-expansion-map with-post-page-generator)
(define-reseting-accessor access-post-style with-post-page-generator)


(bind-defun init-default-with-post-page-generator
    init-with-post-page-generator
    (with-post-page-generator)
    *default-expansion-map*
    *default-post*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer main-page-generator (no-columns-page-generator
                                                    common-generator
                                                    with-post-page-generator)
  ((m-posts-on-main-page-count
    :type (unsigned-byte 8)
    :initarg :posts-on-main-page-count
    :accessor access-posts-on-main-page-count)
   (m-expanding-map
    :accessor access-expanding-map))
  nil
  init-main-page-generator
  main-page-generator)


(bind-defun
    init-default-main-page-generator
    init-main-page-generator
    (main-page-generator)
  10
  *default-expansion-map*)


(defmethod initialize-instance :around ((object main-page-generator) &key)
  (setf (slot-value object 'm-additional-menu-items)
        (list (list "categories" "Categories")))
  (call-next-method))


(define-init-chain init-default-whole-main-page-generator
    main-page-generator
  init-default-common-generator
  init-default-with-post-page-generator
  init-default-main-page-generator)


(define-reseting-accessor access-posts-on-main-page-count m-posts-on-main-page-count)
(define-reseting-accessor access-expanding-map m-expanding-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer posts-list-generator (columns-page-generator
                                                     common-generator)
  ()
  nil
  init-posts-list-generator
  posts-list-generator)


(define-init-chain init-default-whole-posts-list-generator posts-list-generator
    init-default-common-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer categories-list-generator (columns-page-generator
                                                          common-generator)
  ()
  nil
  init-categories-list-generator
  categories-list-generator)


(define-init-chain init-default-whole-categories-list-generator
    categories-list-generator
    init-default-common-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class-with-initializer post-page-generator (no-columns-page-generator
                                                    with-post-page-generator
                                                    common-generator)
  ((m-expanding-map
    :type list
    :accessor access-expanding-map)
   (m-post-style
    :type string
    :accessor access-post-style))
  nil
  init-post-page-generator
  post-page-generator)


(define-reseting-accessor access-expanding-map m-expanding-map)
(define-reseting-accessor access-post-style m-post-style)


(bind-defun init-default-post-page-generator
    init-post-page-generator
    (post-page-generator)
    *default-expansion-map*
    *default-post*)


(define-init-chain init-default-whole-post-page-generator post-page-generator
  init-default-with-post-page-generator
  init-default-post-page-generator
  init-default-common-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod generate-page-from :start ((generator caching-generator)
                                      (object object-with-pages-cache))
  (with-accessors ((m-cached-html access-cached-html)) generator
    (multiple-value-bind (cached found) (gethash (get-cached-page-index object) m-cached-html)
      (get-cached-html (if (or (not found)
                               (edited-before object cached))
                           (setf (gethash (get-cached-page-index object) m-cached-html)
                                 (make-cached-page (call-next-method)))
                           cached)))))


(defmethod generate-page-from ((generator post-page-generator)
                               (object post))
  (standard-page
   (get-style generator)
   (get-menu generator object)
   (access-title object)
    (to-html object (access-expanding-map generator))
    (markup* '(:hr))
    (reduce #'stringify (mapcar (lambda (x) (markup* (list :h3 (access-author x))
                                                     (list :p (access-content x))))
                                (slot-value object 'm-comments)))))


(defmethod generate-page-from ((generator main-page-generator)
                               (object main-container))
  (flet ((generate-posts-html (posts-list)
           (reduce #'stringify
                   (mapcar (lambda (x) (stringify (to-html x (access-expanding-map generator))
                                                  (markup* (list :a :href (format nil
                                                                                  "entry?title=~a"
                                                                                  (slot-value x
                                                                                              'm-id))
                                                                 "Comments"))))
                           posts-list))))
    (standard-page
        (get-style generator)
        (get-menu generator t)
        "Main Page"
      (generate-posts-html (get-most-recent-posts object
                                                  (slot-value generator 'm-posts-on-main-page-count))))))


(defmethod generate-page-from ((generator posts-list-generator)
                               (object posts-container))
  (standard-page
      (get-style generator)
      (get-menu generator object)
      "Posts"
    (reduce #'stringify (mapcar (lambda (x) (markup* (list :li (list :a :href (format nil
                                                                                      "entry?title=~a"
                                                                                      (slot-value x
                                                                                                  'm-id)))
                                                           (slot-value x 'm-title))))
                                (mapcar (lambda (x) (get-post object x))
                                        (get-post-ids object))))))


(defmethod generate-page-from ((generator categories-list-generator)
                               (object main-container))
  (standard-page
      (get-style generator)
      (get-menu generator object)
      "Categories"
    (reduce #'stringify (mapcar (lambda (x) (markup* (list :li (list :a :href (format nil
                                                                                      "category?title=~a"
                                                                                      x))
                                                           x)))

                                (hash-keys (access-categories object))))))


(defmethod generate-page-from ((generator comments-page-generator)
                               (object post))
  (standard-page
      (get-style generator)
      (get-menu generator object)
      "New Post"
    (markup (:h2 "Add a new comment")
            (:form :action "/added-comment" :method "post" :id "addform"
                   (:input :type "hidden" :name "post" :value (slot-value object 'm-id))
                   (:p "Your name" (:br)
                       (:input :type "text" :name "author" :class "txt" :required "true"))
                   (:p "Content" (:br)
                       (:textarea :name "content" :cols 80 :rows 20 :required "true")
                       (:/textarea))
                   (:p (:input :type "submit" :value "Add" :class "btn"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compose-menu ((generator with-menu-page-generator)
                         (object T))
    (with-accessors ((m-additional-menu-items access-additional-menu-items)
                     (m-main-menu-items access-main-menu-items)) generator
      (append m-main-menu-items
              (list 'line)
              m-additional-menu-items)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compose-menu ((generator post-page-generator)
                         (object post))
  (list (list (stringify "add-comment?post=" (slot-value object 'm-id)) "Add comment")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
