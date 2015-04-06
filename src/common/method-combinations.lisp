(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method-combination with-start ()
         ((around (:around))
          (before (:before))
          (start (:start))
          (primary () :required t)
          (after (:after)))
   (flet ((call-methods (methods)
            (mapcar #'(lambda (method)
                        `(call-method ,method))
                    methods)))
     (let ((form (if (or before after (rest primary) start)
                     `(multiple-value-prog1
                        (progn ,@(call-methods before)
                               (call-method ,(first start)
                                            ,(append (rest start) primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary)))))
       (if around
           `(call-method ,(first around)
                         (,@(rest around)
                          (make-method ,form)))
           form))))


(define-method-combination reverse-append ()
  ((primary () :required t))
  (if (endp (cdr primary))
      `(the list (call-method ,(car primary)))
      (cons 'append (mapcar #'(lambda (method) `(the list (call-method ,method)))
                            (reverse primary)))))


(define-method-combination stringify
    :operator stringify
    :identity-with-one-argument t)
