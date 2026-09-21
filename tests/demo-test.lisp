(in-package #:eval-protocol/tests)

(deftest calibration-demo-runs
  (load (asdf:system-relative-pathname "eval-protocol" "examples/calibration.lisp"))
  (ok (typep (eval-protocol/demo:run (make-broadcast-stream))
             'eval-protocol:calibration-gate)))
