(defsystem "eval-protocol"
  :version "0.3.0"
  :description "CLOS evaluation protocol for cl-stack (datasets, scorers, gates)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :properties (:cl-repo
               (:ci (:with ("eval-protocol/judge"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "calibration"))
  :in-order-to ((test-op (test-op "eval-protocol/tests"))))

(defsystem "eval-protocol/judge"
  :version "0.3.0"
  :description "LLM-judge scorer for eval-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("eval-protocol" "llm-protocol")
  :serial t
  :pathname "src/judge"
  :components ((:file "scorer"))
  :in-order-to ((test-op (test-op "eval-protocol/tests"))))

(defsystem "eval-protocol/tests"
  :depends-on ("eval-protocol" "eval-protocol/judge" "llm-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "gates-test")
               (:file "restarts-test")
               (:file "roles-test")
               (:file "calibration-test")
               (:file "judge-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
