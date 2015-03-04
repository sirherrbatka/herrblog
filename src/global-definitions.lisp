(in-package :blog)


(defun make-blog ()
  (make-instance 'main-container
                 :generators (list (list 'main
                                         'generate-main-page
                                         (lambda (x) (let ((posts (get-most-recent-posts x *posts-on-main-page*)))
                                                       (if (endp posts)
                                                           (make-instance 'dummy-timestamp)
                                                           posts))))
                                   (list 'posts
                                         'generate-posts-page
                                         (lambda (x) x))
                                   (list 'categories
                                         'generate-categories-page
                                         (lambda (x) x)))))


(defvar *blog* (make-blog))
(defvar *username* "shka")
(defvar *password* "allcatsaregray")


(defun reset ()
  (setf *blog* (make-blog)))


(defun execute (trans-fn &rest arguments)
  (apply trans-fn arguments)
  (log-transaction (cons trans-fn arguments)
                   *transaction-log*))


(let ((acceptor nil))
  (defun start-server (path)
    (setf *transaction-log*
          (open-transaction-log path
                                (lambda (tra) (apply (car tra)
                                                     (cdr tra)))))
    (start (if (null acceptor)
               (setf acceptor (make-instance 'easy-acceptor
                                             :port 8080))
               acceptor)))


  (defun stop-server ()
    (close-transaction-log *transaction-log*)
    (reset-sessions acceptor)
    (reset)
    (unless (null acceptor)
      (stop acceptor))))
