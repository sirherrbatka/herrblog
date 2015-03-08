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


(defmacro standard-page (style menu title &body body)
  `(html5 (:html5 (:head
                   (:title ,title)
                   (:meta :charset "utf-8")
                   (:style (raw ,style)))
                  (:body
                   (:header
                    (:h1 "some tytul"))
                   (raw ,menu)
                   (:main (raw ,@body))))))
