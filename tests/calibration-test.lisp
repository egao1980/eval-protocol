(in-package #:eval-protocol/tests)

(defun %approx= (a b &optional (tol 1e-9))
  (<= (abs (- a b)) tol))

(deftest-parametrize brier-known-mass
    ((mass outcome expected)
     :ids ("perfect-true" "perfect-miss" "binary-4/5"
           "uniform-ternary" "true-false-keys" "keyword-outcome")
     (((t . 1) (nil . 0)) t 0)
     (((t . 1) (nil . 0)) nil 2)
     (((t . 4/5) (nil . 1/5)) t 2/25)
     (((:a . 1/3) (:b . 1/3) (:c . 1/3)) :a 2/3)
     (((:true . 1) (:false . 0)) t 0)
     (((t . 1) (nil . 0)) :true 0))
  (ok (= expected (eval-protocol:brier-score mass outcome))))

(deftest-parametrize log-loss-known-mass
    ((mass outcome p-true)
     :ids ("perfect" "half" "missing-clipped")
     (((t . 1) (nil . 0)) t 1)
     (((t . 1/2) (nil . 1/2)) t 1/2)
     (((t . 0) (nil . 1)) t 0))
  (let ((expected (- (log (max 1e-12 p-true)))))
    (ok (%approx= expected (eval-protocol:log-loss mass outcome)))))

(deftest-parametrize ece-known-predictions
    ((predictions expected)
     :ids ("empty" "perfect-ones" "all-0.9-wrong" "two-bins" "mid-bin-half")
     (nil 0)
     (((1 . t) (1 . t)) 0)
     (((9/10 . nil) (9/10 . nil)) 9/10)
     (((1/10 . nil) (1/10 . nil) (9/10 . t) (9/10 . t)) 1/10)
     (((1/4 . t) (1/4 . nil)) 1/4))
  (ok (= expected (eval-protocol:expected-calibration-error predictions))))

(deftest reliability-bins-equal-width-and-one
  (let* ((preds '((0 . nil) (1/10 . t) (1 . t)))
         (bins (eval-protocol:reliability-bins preds :bins 10))
         (first (first bins))
         (second (second bins))
         (last (car (last bins))))
    (ok (= 10 (length bins)))
    (ok (= 0 (getf first :lo)))
    (ok (= 1/10 (getf first :hi)))
    (ok (= 1 (getf first :count)))
    (ok (= 0 (getf first :mean-p)))
    (ok (= 0 (getf first :accuracy)))
    (ok (= 1 (getf second :count)))
    (ok (= 1/10 (getf second :mean-p)))
    (ok (= 1 (getf second :accuracy)))
    (ok (= 9/10 (getf last :lo)))
    (ok (= 1 (getf last :hi)))
    (ok (= 1 (getf last :count)))
    (ok (= 1 (getf last :mean-p)))
    (ok (= 1 (getf last :accuracy)))))

(deftest automation-at-budget-max-coverage
  ;; Highest-confidence wrong case cannot be automated at max-error 0.
  (ok (zerop (eval-protocol:automation-at-budget
              '((9/10 . nil) (4/5 . t))
              :max-error 0)))
  ;; All correct: entire cohort is automatable.
  (ok (= 1 (eval-protocol:automation-at-budget
            '((9/10 . t) (4/5 . t) (1/2 . t))
            :max-error 0)))
  ;; Act on the two high-p correct cases; including 0.4 (wrong) blows the budget.
  (ok (= 2/3 (eval-protocol:automation-at-budget
              '((95/100 . t) (8/10 . t) (4/10 . nil))
              :max-error 1/20)))
  ;; Explicit threshold-on uses that t.
  (ok (= 1/2 (eval-protocol:automation-at-budget
              '((9/10 . t) (1/2 . nil))
              :max-error 1
              :threshold-on 9/10)))
  (ok (zerop (eval-protocol:automation-at-budget
              '((9/10 . nil))
              :max-error 0
              :threshold-on 9/10))))

(deftest option-order-spread-max-minus-min
  (let ((spread (eval-protocol:option-order-spread
                 (list '((:a . 7/10) (:b . 3/10))
                       '((:a . 4/10) (:b . 6/10))))))
    (ok (= 3/10 (cdr (assoc :a spread))))
    (ok (= 3/10 (cdr (assoc :b spread))))))

(deftest isolation-delta-packed-vs-separate
  (let* ((packed '(:answers ((:id "q1" :mass ((:yes . 4/5) (:no . 1/5)))
                             (:id "q2" :mass ((:a . 1/2) (:b . 1/2))))))
         (same '(:answers ((:id "q1" :mass ((:yes . 4/5) (:no . 1/5)))
                           (:id "q2" :mass ((:a . 1/2) (:b . 1/2))))))
         (drift '(:answers ((:id "q1" :mass ((:yes . 1/2) (:no . 1/2)))
                            (:id "q2" :mass ((:a . 1/2) (:b . 1/2))))))
         (alist-packed '(("q1" . ((:yes . 4/5) (:no . 1/5)))))
         (alist-sep '(("q1" . ((:yes . 1/2) (:no . 1/2))))))
    (ok (zerop (eval-protocol:isolation-delta packed same)))
    (ok (= 3/10 (eval-protocol:isolation-delta packed drift)))
    (ok (= 3/10 (eval-protocol:isolation-delta alist-packed alist-sep)))))

(deftest drift-report-brier-ece-deltas
  (let* ((baseline '(:brier 1/10 :ece 1/20 :confidence 99/100))
         (candidate '(:brier 12/100 :ece 1/10 :confidence 1/100))
         (report (eval-protocol:make-drift-report
                  :baseline baseline :candidate candidate))
         (via-fn (eval-protocol:drift-report baseline candidate)))
    (ok (eval-protocol:drift-report-p report))
    (ok (= 2/100 (eval-protocol:drift-report-brier-delta report)))
    (ok (= 1/20 (eval-protocol:drift-report-ece-delta report)))
    (ok (= (eval-protocol:drift-report-brier-delta report)
           (eval-protocol:drift-report-brier-delta via-fn)))
    (ok (equal '(:brier :ece) (eval-protocol:drift-report-metrics report)))))

(deftest calibration-gate-brier-ece
  (let ((gate (eval-protocol:make-calibration-gate))
        (loose (eval-protocol:make-calibration-gate :brier-delta 1/10 :ece-delta 1/10))
        (baseline '(:brier 1/10 :ece 1/20))
        (same '(:brier 1/10 :ece 1/20))
        (worse '(:brier 2/10 :ece 1/10))
        (better '(:brier 1/20 :ece 1/100)))
    (ok (eval-protocol:gate-passes-p gate baseline same))
    (ok (eval-protocol:gate-passes-p gate baseline better))
    (ng (eval-protocol:gate-passes-p gate baseline worse))
    (ok (eval-protocol:gate-passes-p loose baseline worse))
    (ok (eval-protocol:gate-passes-p
         gate
         (eval-protocol:make-drift-report :baseline baseline :candidate same)
         t))))

(deftest option-order-gate-max-spread
  (let* ((stable (list '((:a . 1/2) (:b . 1/2))
                       '((:a . 1/2) (:b . 1/2))))
         (unstable (list '((:a . 9/10) (:b . 1/10))
                         '((:a . 1/10) (:b . 9/10))))
         (strict (eval-protocol:make-option-order-gate :max-spread 0))
         (wide (eval-protocol:make-option-order-gate :max-spread 1)))
    (ok (eval-protocol:gate-passes-p strict nil stable))
    (ng (eval-protocol:gate-passes-p strict nil unstable))
    (ok (eval-protocol:gate-passes-p wide nil unstable))
    (ok (eval-protocol:gate-passes-p
         strict nil (eval-protocol:option-order-spread stable)))))

(deftest-parametrize choice-concentration-formula
    ((pmax k expected)
     :ids ("k1" "uniform-binary" "sure-ternary" "mid-binary")
     (1 1 1)
     (1/2 2 0)
     (1 3 1)
     (3/4 2 1/2))
  (ok (= expected (eval-protocol:choice-concentration pmax k))))

(deftest concentration-is-not-accuracy
  (ok (eval-protocol:concentration-as-accuracy-forbidden))
  (ok (eval-protocol:confidence-is-not-accuracy-p))
  (ng (eval-protocol:calibration-treats-concentration-as-accuracy-p))
  ;; Gates consume Brier/ECE. A :confidence field must not change the verdict.
  (let ((gate (eval-protocol:make-calibration-gate))
        (baseline '(:brier 1/10 :ece 1/20 :confidence 99/100))
        (candidate '(:brier 1/10 :ece 1/20 :confidence 1/100)))
    (ok (eval-protocol:gate-passes-p gate baseline candidate)))
  ;; Using concentration as if it were P(correct) is the forbidden path.
  (ng (eval-protocol:calibration-treats-concentration-as-accuracy-p)))
