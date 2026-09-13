(in-package #:eval-protocol/tests)

(defun %case-result (input expected value &key metadata (verdict nil))
  (eval-protocol:make-eval-case-result
   :case (eval-protocol:make-eval-case
          :input input :expected expected :metadata metadata)
   :actual expected
   :score (eval-protocol:make-eval-score
           :value value
           :verdict (or verdict (if (= value 1) :pass :fail))
           :rationale "")))

(defun %run (results)
  (eval-protocol:make-eval-run :results results))

(deftest mean-improvement-gate
  (let ((baseline (%run (list (%case-result "a" "a" 0))))
        (better (%run (list (%case-result "a" "a" 1))))
        (same (%run (list (%case-result "a" "a" 0))))
        (gate (eval-protocol:make-mean-improvement-gate)))
    (ok (eval-protocol:gate-passes-p gate baseline better))
    (ng (eval-protocol:gate-passes-p gate baseline same))
    (ng (eval-protocol:gate-passes-p
         (eval-protocol:make-mean-improvement-gate :delta 1)
         baseline better))))

(deftest no-critical-regression-gate
  (let* ((critical '(:tags (:critical)))
         (baseline (%run (list (%case-result "crit" "ok" 1 :metadata critical)
                               (%case-result "other" "x" 0))))
         (ok-cand (%run (list (%case-result "crit" "ok" 1 :metadata critical)
                              (%case-result "other" "x" 1))))
         (lost (%run (list (%case-result "crit" "ok" 0 :metadata critical)
                           (%case-result "other" "x" 1))))
         (flagged (%run (list (%case-result "crit" "ok" 0 :metadata '(:critical t))
                              (%case-result "other" "x" 1))))
         (gate (eval-protocol:make-no-critical-regression-gate)))
    (ok (eval-protocol:gate-passes-p gate baseline ok-cand))
    (ng (eval-protocol:gate-passes-p gate baseline lost))
    (ng (eval-protocol:gate-passes-p gate baseline flagged))))

(deftest default-promotion-gate-composes-both
  (let* ((critical '(:tags (:critical)))
         (baseline (%run (list (%case-result "crit" "ok" 1 :metadata critical)
                               (%case-result "other" "x" 0))))
         (mean-win-but-lost-critical
          (%run (list (%case-result "crit" "ok" 0 :metadata critical)
                      (%case-result "other" "x" 1))))
         (mean-win-and-safe
          (%run (list (%case-result "crit" "ok" 1 :metadata critical)
                      (%case-result "other" "x" 1))))
         (gate (eval-protocol:make-default-promotion-gate))
         (composed (eval-protocol:make-composed-gate
                    :policies (list (eval-protocol:make-mean-improvement-gate)
                                    (eval-protocol:make-no-critical-regression-gate)))))
    (ng (eval-protocol:gate-passes-p gate baseline mean-win-but-lost-critical))
    (ok (eval-protocol:gate-passes-p gate baseline mean-win-and-safe))
    (ng (eval-protocol:gate-passes-p composed baseline mean-win-but-lost-critical))
    (ok (eval-protocol:gate-passes-p composed baseline mean-win-and-safe))))
