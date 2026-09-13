(in-package #:eval-protocol)

(define-condition eval-error (error)
  ((message :initarg :message :reader eval-error-message :initform nil))
  (:report (lambda (c s)
             (format s "eval error~@[: ~a~]" (eval-error-message c)))))

(define-condition scorer-error (eval-error)
  ((cause :initarg :cause :reader scorer-error-cause :initform nil)
   (case :initarg :case :reader scorer-error-case :initform nil)
   (scorer :initarg :scorer :reader scorer-error-scorer :initform nil))
  (:report (lambda (c s)
             (format s "eval scorer error~@[: ~a~]~@[ (cause: ~a)~]"
                     (eval-error-message c)
                     (scorer-error-cause c)))))
