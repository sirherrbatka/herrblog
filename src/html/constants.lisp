(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-posts-on-main-page* 5)

(defvar *default-menu-items* '(("blog" "Main Page")
                               ("posts" "All Posts")))

(defvar *default-html* "html { font-family: 'Clear Sans'; background: #eee; }")

(defvar *default-source* "code { font-family: monospace; }")

(defvar *default-body* "body {overflow: auto; background: #fff; padding: 1.250em; box-shadow: 0 0.125em 0.188em #ccc; max-width: 750em; margin: 1.875em;}")

(defvar *default-body-after* "body:after { clear: both; width: 0.063em; height: 0.063em; display: block; visibility: hidden; content: \".\"; }")

(defvar *default-header* "#header { color: #333; }")

(defvar *default-h1* "h1 { font-size: 3em; margin: 0; }")

(defvar *default-menu* "#menu { width: 23%; float: left; padding: 1%; }")

(defvar *default-main* "main { width: 73%; padding: 1%; float: right; }")

(defvar *default-article* "article { margin: 0 0 2.5em 0; }")

(defvar *default-article-footer* "article footer { font-size: 0.8em; color: #999; }")

(defvar *default-display* "main, article, header, footer { display: block; }")

(defvar *default-columns-main* "main { width: 73%; padding: 1%; float: right; webkit-columns: 3; -moz-columns: 3; columns: 3; }")

(defvar *default-line* "hr { border: 0; height: 0; width: 75% margin-left: auto; margin-right: auto; border-top: 0.125em solid rgba(0, 0, 0, 0.1); border-bottom: 0.063em solid rgba(255, 255, 255, 0.3); }")

(defvar *default-post* "")

(defvar *default-link* "a { text-decoration: none; color: inherit; outline: none; }")
