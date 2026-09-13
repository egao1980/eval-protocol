(in-package #:eval-protocol/tests)

(defun %boom-dataset ()
  (eval-protocol:make-eval-dataset
   :name "boom"
   :cases (list (eval-protocol:make-eval-case :input 1 :expected 1)
                (eval-protocol:make-eval-case :input :boom :expected 0))))

(defun %boom-target (x)
  (if (eq x :boom)
      (error "boom")
      x))

(deftest scorer-error-signals
  (ok (signals (eval-protocol:run-eval (%boom-dataset) #'%boom-target)
               'eval-protocol:scorer-error)))

(deftest skip-case-restart
  (let ((run (handler-bind ((eval-protocol:scorer-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'eval-protocol:skip-case))))
               (eval-protocol:run-eval (%boom-dataset) #'%boom-target))))
    (ok (= 1 (eval-protocol:eval-run-n run)))
    (ok (= 1 (eval-protocol:eval-run-pass-count run)))
    (ok (= 1 (eval-protocol:eval-run-mean run)))
    (ok (eval-protocol:eval-case-result-skipped-p
         (second (eval-protocol:eval-run-results run))))))

(deftest score-as-failure-restart
  (let ((run (handler-bind ((eval-protocol:scorer-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'eval-protocol:score-as-failure))))
               (eval-protocol:run-eval (%boom-dataset) #'%boom-target))))
    (ok (= 2 (eval-protocol:eval-run-n run)))
    (ok (= 1 (eval-protocol:eval-run-pass-count run)))
    (ok (= 1/2 (eval-protocol:eval-run-mean run)))
    (ok (eq :fail
            (eval-protocol:eval-score-verdict
             (eval-protocol:eval-case-result-score
              (second (eval-protocol:eval-run-results run))))))))

(deftest retry-case-restart
  (let ((attempts 0)
        (run nil))
    (setf run
          (handler-bind ((eval-protocol:scorer-error
                          (lambda (c)
                            (declare (ignore c))
                            (if (< attempts 2)
                                (invoke-restart 'eval-protocol:retry-case)
                                (invoke-restart 'eval-protocol:score-as-failure)))))
            (eval-protocol:run-eval
             (%boom-dataset)
             (lambda (x)
               (when (eq x :boom)
                 (incf attempts)
                 (error "boom"))
               x))))
    (ok (>= attempts 2))
    (ok (= 2 (eval-protocol:eval-run-n run)))))
