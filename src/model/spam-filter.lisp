(in-package :herrblog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-questions-dictionary* (make-hash-table :test 'equal))


(defun add-to-question-dictionary (question answer dictionary)
  (declare (type string question answer)
           (type hash-table dictionary))
  (add-to-hash (string-upcase question) (string-upcase answer) dictionary))


(add-to-question-dictionary "What is the capital of France?" "Paris" *default-questions-dictionary*)
(add-to-question-dictionary "What is the capital of Germany?" "Berlin" *default-questions-dictionary*)
(add-to-question-dictionary "How is the one-dimensional array called?" "vector" *default-questions-dictionary*)
(add-to-question-dictionary "Does circle has any angles?" "no" *default-questions-dictionary*)
(add-to-question-dictionary "Does square has more angles then a triangle?" "yes" *default-questions-dictionary*)


(defun correct-answer (question answer dictionary)
  (declare (type string question answer)
           (type hash-map dictionary))
  (the symbol (equal (gethash (string-upcase question) dictionary) (string-upcase answer))))
