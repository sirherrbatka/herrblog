(in-package :blog)

(defconstant *default-posts-on-main-page* 5)

(defconstant *default-menu-items* '(("blog" "Main Page")
                                    ("posts" "All Posts")))

(defconstant *default-html* "html { font-family: 'Clear Sans'; background: #eee; }")

(defconstant *default-body* "body { background: #fff; padding: 20px; box-shadow: 0 2px 3px #ccc; max-width: 1200px; overflow: hidden; margin: 30px auto; }")

(defconstant *default-body-after* "body:after { clear: both; width: 1px; height: 1px; display: block; visibility: hidden; content: \".\"; }")

(defconstant *default-header* "#header { color: #333; }")

(defconstant *default-h1* "h1 { font-size: 3em; margin: 0; }")

(defconstant *default-menu* "#menu { width: 23%; float: left; padding: 1%; }")

(defconstant *default-main* "main { width: 73%; padding: 1%; float: right; }")

(defconstant *default-article* "article { margin: 0 0 40px 0; }")

(defconstant *default-article-footer* "article footer { font-size: .8em; color: #999; }")

(defconstant *default-display* "main, article, header, footer { display: block; }")

(defconstant *default-columns-main* "main { width: 73%; padding: 1%; float: right; webkit-columns: 3; -moz-columns: 3; columns: 3; }" )

(defconstant *default-line* "hr { border: 0; height: 0; width: 75% margin-left: auto; margin-right: auto; border-top: 1px solid rgba(0, 0, 0, 0.1); border-bottom: 1px solid rgba(255, 255, 255, 0.3); }")
