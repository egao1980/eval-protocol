(defpackage #:eval-protocol
  (:use #:cl)
  (:nicknames #:stack-eval)
  (:export #:eval-error
           #:eval-error-message
           #:scorer-error
           #:scorer-error-cause
           #:scorer-error-case
           #:scorer-error-scorer
           #:skip-case
           #:retry-case
           #:score-as-failure

           #:eval-case
           #:eval-case-p
           #:make-eval-case
           #:eval-case-input
           #:eval-case-expected
           #:eval-case-metadata

           #:eval-dataset
           #:eval-dataset-p
           #:make-eval-dataset
           #:eval-dataset-name
           #:eval-dataset-cases
           #:eval-dataset-version
           #:eval-dataset-provenance
           #:add-case
           #:dump-dataset
           #:load-dataset

           #:eval-score
           #:eval-score-p
           #:make-eval-score
           #:eval-score-value
           #:eval-score-verdict
           #:eval-score-rationale

           #:eval-case-result
           #:eval-case-result-p
           #:make-eval-case-result
           #:eval-case-result-case
           #:eval-case-result-actual
           #:eval-case-result-score
           #:eval-case-result-skipped-p

           #:eval-run
           #:eval-run-p
           #:make-eval-run
           #:eval-run-dataset
           #:eval-run-results
           #:eval-run-mean
           #:eval-run-n
           #:eval-run-pass-count

           #:score-case
           #:run-eval
           #:eval-report

           #:eval-scorer
           #:exact-match-scorer
           #:make-exact-match-scorer
           #:contains-scorer
           #:make-contains-scorer
           #:numeric-tolerance-scorer
           #:make-numeric-tolerance-scorer
           #:numeric-tolerance-scorer-tolerance
           #:rubric-scorer
           #:make-rubric-scorer
           #:rubric-scorer-predicates

           #:eval-gate
           #:gate-passes-p
           #:mean-improvement-gate
           #:make-mean-improvement-gate
           #:mean-improvement-gate-delta
           #:no-critical-regression-gate
           #:make-no-critical-regression-gate
           #:composed-gate
           #:make-composed-gate
           #:composed-gate-policies
           #:default-promotion-gate
           #:make-default-promotion-gate

           #:llm-judge-scorer
           #:make-llm-judge-scorer
           #:llm-judge-scorer-backend
           #:llm-judge-scorer-prompt))

(in-package #:eval-protocol)
