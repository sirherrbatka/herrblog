(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun standard-menu (&rest body)
  (append (list :ui :role "navigation" :id "menu")
          (mapcar (lambda (x) (cond ((eq x 'line) (list :hr))
                                    ((eq x 'no-line) nil)
                                    (t
                                     (list :li
                                           (list :a
                                                 :href (car x)
                                                 (cadr x))))))
                  body)))


(defmacro standard-page (style menu title &body body)
  `(html5 (:html5
           (:link :href "http://fonts.googleapis.com/css?family=Oxygen" :rel "stylesheet" :type "text/css")
           (:head (:title ,title)
                   (:meta :charset "utf-8")
                   (:style (raw ,style)))
                  (:body
                   (:header
                    (:h1 "Metapatterns"))
                   (raw ,menu)
                   (:main
                    (raw ,@body))))))
