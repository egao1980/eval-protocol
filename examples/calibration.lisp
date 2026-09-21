;;;; Calibration demo — Brier / ECE / drift; concentration is not accuracy.
;;;;   sbcl --load examples/calibration.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eval-protocol)
    (require :asdf)
    (asdf:load-system "eval-protocol")))

(defpackage #:eval-protocol/demo
  (:use #:cl #:eval-protocol)
  (:export #:run))

(in-package #:eval-protocol/demo)

(defun run (&optional (stream *standard-output*))
  "Print proper scores + a passing calibration gate. Returns the gate."
  (let* ((mass '((:allow . 4/5) (:deny . 1/5)))
         (brier (brier-score mass :allow))
         (ll (log-loss mass :allow))
         (pairs '((0.8 . t) (0.8 . t) (0.2 . nil) (0.2 . nil)))
         (ece (expected-calibration-error pairs))
         (conc (choice-concentration 4/5 2))
         (p-allow 4/5)
         (gate (make-calibration-gate))
         (baseline (list :brier brier :ece ece))
         (candidate (list :brier brier :ece ece)))
    (format stream "~&; brier=~s log-loss=~s ece=~s~%" brier ll ece)
    (format stream "~&; concentration=~s p(allow)=~s (must differ)~%" conc p-allow)
    (assert (/= conc p-allow))
    (assert (confidence-is-not-accuracy-p))
    (assert (not (calibration-treats-concentration-as-accuracy-p)))
    (assert (gate-passes-p gate baseline candidate))
    (format stream "~&; calibration-gate pass vs pinned cohort~%")
    gate))

#+sbcl
(when (and *load-truename*
           (equal (pathname-name *load-truename*) "calibration")
           (find "examples/calibration.lisp" sb-ext:*posix-argv* :test #'search))
  (run)
  (uiop:quit 0))
