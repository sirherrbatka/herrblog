(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *transaction-log* nil)

(defun make-blog ()
  (make-instance 'main-container))


(defvar *blog* (make-blog))


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
