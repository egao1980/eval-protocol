(in-package #:eval-protocol/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (asdf:system-relative-pathname "eval-protocol" "examples/calibration.lisp")))

(deftest calibration-demo-runs
  (ok (typep (eval-protocol/demo:run (make-broadcast-stream))
             'eval-protocol:calibration-gate)))
