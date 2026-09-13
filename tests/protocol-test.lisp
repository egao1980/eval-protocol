(in-package #:eval-protocol/tests)

(defun %scorer (kind &key (tolerance 1e-6) predicates)
  (ecase kind
    (:exact-match (eval-protocol:make-exact-match-scorer))
    (:contains (eval-protocol:make-contains-scorer))
    (:numeric (eval-protocol:make-numeric-tolerance-scorer :tolerance tolerance))
    (:rubric (eval-protocol:make-rubric-scorer :predicates predicates))))

(deftest-parametrize scorer-matrix
    ((kind actual expected pass-p)
     :ids ("exact-hit" "exact-miss" "exact-list"
           "contains-hit" "contains-miss" "contains-symbol"
           "numeric-hit" "numeric-miss" "numeric-default-tol")
     (:exact-match "hello" "hello" t)
     (:exact-match "hello" "world" nil)
     (:exact-match '(1 2) '(1 2) t)
     (:contains "the quick brown fox" "quick" t)
     (:contains "the quick brown fox" "slow" nil)
     (:contains :FOOBAR "FOO" t)
     (:numeric 1.0 1.0 t)
     (:numeric 1.0 2.0 nil)
     (:numeric 1.0 1.0000001 t))
  (let* ((case (eval-protocol:make-eval-case :input :x :expected expected))
         (score (eval-protocol:score-case (%scorer kind) case actual)))
    (ok (eval-protocol:eval-score-p score))
    (ok (eq (if pass-p :pass :fail)
            (eval-protocol:eval-score-verdict score)))
    (if pass-p
        (ok (= 1 (eval-protocol:eval-score-value score)))
        (ok (zerop (eval-protocol:eval-score-value score))))))

(deftest rubric-all-predicates
  (let* ((scorer (eval-protocol:make-rubric-scorer
                  :predicates (list (lambda (case actual)
                                      (declare (ignore case))
                                      (evenp actual))
                                    (lambda (case actual)
                                      (declare (ignore case))
                                      (> actual 0)))))
         (pass (eval-protocol:score-case
                scorer (eval-protocol:make-eval-case :input 1 :expected 2) 2))
         (fail (eval-protocol:score-case
                scorer (eval-protocol:make-eval-case :input 1 :expected 3) 3)))
    (ok (eq :pass (eval-protocol:eval-score-verdict pass)))
    (ok (= 1 (eval-protocol:eval-score-value pass)))
    (ok (eq :fail (eval-protocol:eval-score-verdict fail)))
    (ok (= 1/2 (eval-protocol:eval-score-value fail)))))

(deftest make-eval-score-checks-range
  (ok (signals (eval-protocol:make-eval-score :value 2 :verdict :pass)
               'type-error))
  (ok (signals (eval-protocol:make-eval-score :value -0.1 :verdict :fail)
               'type-error)))

(deftest dataset-versioning-and-immutability
  (let* ((c1 (eval-protocol:make-eval-case :input "a" :expected "a"))
         (c2 (eval-protocol:make-eval-case :input "b" :expected "b"
                                           :metadata '(:source :human)))
         (d1 (eval-protocol:make-eval-dataset :name "suite" :cases (list c1)))
         (v1 (eval-protocol:eval-dataset-version d1))
         (d2 (eval-protocol:add-case d1 c2 :source :human-feedback))
         (d1-again (eval-protocol:make-eval-dataset :name "suite" :cases (list c1))))
    (ok (stringp v1))
    (ok (= 16 (length v1)))
    (ok (= 1 (length (eval-protocol:eval-dataset-cases d1))))
    (ok (null (eval-protocol:eval-dataset-provenance d1)))
    (ok (= 2 (length (eval-protocol:eval-dataset-cases d2))))
    (ok (not (equal v1 (eval-protocol:eval-dataset-version d2))))
    (ok (equal :human-feedback
               (find :human-feedback (eval-protocol:eval-dataset-provenance d2))))
    (ok (equal v1 (eval-protocol:eval-dataset-version d1-again)))
    (ok (equal "suite" (eval-protocol:eval-dataset-name d2)))))

(deftest dump-load-dataset-sexp
  (let* ((d (eval-protocol:make-eval-dataset
             :name "round"
             :cases (list (eval-protocol:make-eval-case
                           :input 1 :expected 1
                           :metadata '(:tags (:critical))))
             :provenance '(:human-feedback)))
         (text (eval-protocol:dump-dataset d))
         (loaded (eval-protocol:load-dataset text)))
    (ok (stringp text))
    (ok (equal (eval-protocol:eval-dataset-name d)
               (eval-protocol:eval-dataset-name loaded)))
    (ok (equal (eval-protocol:eval-dataset-version d)
               (eval-protocol:eval-dataset-version loaded)))
    (ok (equal '(:human-feedback)
               (eval-protocol:eval-dataset-provenance loaded)))
    (ok (equal '(:tags (:critical))
               (eval-protocol:eval-case-metadata
                (first (eval-protocol:eval-dataset-cases loaded)))))))

(deftest run-eval-aggregates
  (let* ((ds (eval-protocol:make-eval-dataset
              :name "id"
              :cases (list (eval-protocol:make-eval-case :input 1 :expected 2)
                           (eval-protocol:make-eval-case :input 3 :expected 6)
                           (eval-protocol:make-eval-case :input 4 :expected 0))))
         (run (eval-protocol:run-eval ds (lambda (x) (* x 2)))))
    (ok (= 3 (eval-protocol:eval-run-n run)))
    (ok (= 2 (eval-protocol:eval-run-pass-count run)))
    (ok (= 2/3 (eval-protocol:eval-run-mean run)))
    (let ((report (eval-protocol:eval-report run)))
      (ok (eq :eval-run (first report)))
      (ok (= 3 (getf (rest report) :n))))))
