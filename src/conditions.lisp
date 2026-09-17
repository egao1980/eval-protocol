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

(define-condition holdout-admission-error (eval-error)
  ((source :initarg :source :reader holdout-admission-error-source :initform nil)
   (role :initarg :role :reader holdout-admission-error-role :initform nil)
   (case :initarg :case :reader holdout-admission-error-case :initform nil)
   (dataset :initarg :dataset :reader holdout-admission-error-dataset :initform nil))
  (:report (lambda (c s)
             (format s "production/feedback source ~s cannot enter :holdout~
~@[: ~a~]"
                     (holdout-admission-error-source c)
                     (eval-error-message c)))))

(define-condition holdout-overlap-error (eval-error)
  ((train :initarg :train :reader holdout-overlap-error-train :initform nil)
   (holdout :initarg :holdout :reader holdout-overlap-error-holdout :initform nil)
   (keys :initarg :keys :reader holdout-overlap-error-keys :initform nil))
  (:report (lambda (c s)
             (format s "train/search data overlaps promotion holdout~
~@[ (~a keys)~]~@[: ~a~]"
                     (length (holdout-overlap-error-keys c))
                     (eval-error-message c)))))

(define-condition paired-trial-gate-error (eval-error)
  ((n :initarg :n :reader paired-trial-gate-error-n :initform nil)
   (min-sample :initarg :min-sample :reader paired-trial-gate-error-min-sample
               :initform nil)
   (confidence :initarg :confidence :reader paired-trial-gate-error-confidence
               :initform nil)
   (confidence-threshold :initarg :confidence-threshold
                         :reader paired-trial-gate-error-confidence-threshold
                         :initform nil)
   (result :initarg :result :reader paired-trial-gate-error-result :initform nil))
  (:report (lambda (c s)
             (format s "paired-trial promote refused (n=~a min-sample=~a ~
confidence=~a threshold=~a)~@[: ~a~]"
                     (paired-trial-gate-error-n c)
                     (paired-trial-gate-error-min-sample c)
                     (paired-trial-gate-error-confidence c)
                     (paired-trial-gate-error-confidence-threshold c)
                     (eval-error-message c)))))
