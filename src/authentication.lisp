(in-package :herrblog)


(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username *username*) (string= password *password*))
            ,@body)
           (t (hunchentoot:require-authorization "blog")))))
