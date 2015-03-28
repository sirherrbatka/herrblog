(in-package :blog)

(defgeneric generate-page-from (generator object)
  (:method-combination with-start))
(defgeneric to-html (p rules))
(defgeneric edited-before (a b))
(defgeneric created-before (a b))
(defgeneric add-post (blog-main new-entry))
(defgeneric remove-post (container id))
(defgeneric sort-content (container))
(defgeneric get-post (blog id))
(defgeneric get-page (object identifer))
(defgeneric get-most-recent-posts (blog count))
(defgeneric new-category (id blog))
(defgeneric get-category (blog id))
(defgeneric get-style (generator)
  (:method-combination stringify))
(defgeneric set-cached-html (page value))
(defgeneric reset-cache (object))
(defgeneric get-menu (generator object))
(defgeneric update-timestamp (object))
(defgeneric compose-menu (generator object)
  (:method-combination reverse-append))
