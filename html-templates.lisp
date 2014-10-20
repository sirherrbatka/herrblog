(in-package :blog)


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


(let ((default-menu (list (list "blog"
                                "Main Page")
                          (list "posts"
                                "All Posts"))))

  (defun get-menu (&rest other)
    (markup* (apply 'standard-menu (append default-menu
                                           (list (if (null other) 'no-line 'line))
                                           other)))))


(defmacro standard-page (style menu title &body body)
  `(html5 (:head
           (:title ,title)
           (:meta :charset "utf-8")
           (:style (raw ,style)))
          (:body
           (:header
            (:h1 "some tytul"))
           (raw ,menu)
           (:main (raw ,@body)))))
