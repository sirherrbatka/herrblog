(in-package :blog)

(defvar *posts-on-main-page* 5)

(defvar *transaction-log* nil)

(let ((html "html {
 font-family: 'Clear Sans';
 background: #eee;
 }")
      (body "body {
  background: #fff;
  padding: 20px;
  box-shadow: 0 2px 3px #ccc;
  max-width: 1200px;
  overflow: hidden;
  margin: 30px auto;
}")
      (body-after "body:after {
  clear: both;
  width: 1px;
  height: 1px;
  display: block;
  visibility: hidden;
  content: \".\";
}")
      (header "#header {
  color: #333;
}")
      (h1 "h1 {
  font-size: 3em;
  margin: 0;
}")
      (menu "#menu {
  width: 23%;
  float: left;
  padding: 1%;
}")
      (main "main {
  width: 73%;
  padding: 1%;
  float: right;
}")
      (article "article {
  margin: 0 0 40px 0;
}")
      (article-footer "article footer {
  font-size: .8em;
  color: #999;
}")
      (display "main, article, header, footer {
  display: block;
}")

      (columns-main "main {
  width: 73%;
  padding: 1%;
  float: right;
  webkit-columns: 3;
  -moz-columns: 3;
        columns: 3;
}"))

  (defun get-style (&key (columns-for-main nil))
    (stringify html
               body
               body-after
               header
               h1
               menu
               (if columns-for-main columns-main main)
               article
               article-footer
               display)))
