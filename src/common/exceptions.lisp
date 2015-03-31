(in-package :blog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition post-exception (error)
  ())


(define-condition post-not-found (post-exception)
 ())


(define-condition post-cant-be-added (post-exception)
  ((m-reason
    :initarg :reason
    :reader get-reason)))


(define-condition category-exception (error)
  ())


(define-condition category-cant-be-added (category-exception)
  ((m-reason
    :initarg :reason
    :reader get-reason)))


(define-condition page-not-found (error)
  ())
