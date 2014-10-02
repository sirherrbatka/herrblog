(in-package :blog)


(defgeneric to-html (p))
(defgeneric edited-before (a b))
(defgeneric created-before (a b))
(defgeneric add-post (blog-main new-entry))
(defgeneric sort-content (container))
(defgeneric get-post (blog id))
(defgeneric get-page (object identifer))
(defgeneric generate-posts-page (blog))
(defgeneric generate-main-page (blog))
(defgeneric generate-post-page (p))
(defgeneric get-most-recent-posts (blog count))
