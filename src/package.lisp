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
           #:holdout-admission-error
           #:holdout-admission-error-source
           #:holdout-admission-error-role
           #:holdout-admission-error-case
           #:holdout-admission-error-dataset
           #:force-holdout-admission
           #:holdout-overlap-error
           #:holdout-overlap-error-train
           #:holdout-overlap-error-holdout
           #:holdout-overlap-error-keys
           #:paired-trial-gate-error
           #:paired-trial-gate-error-n
           #:paired-trial-gate-error-min-sample
           #:paired-trial-gate-error-confidence
           #:paired-trial-gate-error-confidence-threshold
           #:paired-trial-gate-error-result

           #:eval-case
           #:eval-case-p
           #:make-eval-case
           #:eval-case-input
           #:eval-case-expected
           #:eval-case-metadata
           #:eval-case-role
           #:eval-case-source
           #:eval-case-parent-version
           #:eval-case-lineage

           #:eval-dataset
           #:eval-dataset-p
           #:make-eval-dataset
           #:eval-dataset-name
           #:eval-dataset-cases
           #:eval-dataset-version
           #:eval-dataset-provenance
           #:eval-dataset-role
           #:dataset-role-p
           #:add-case
           #:dataset-split
           #:assert-no-holdout-overlap
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

           #:paired-trial-result
           #:paired-trial-result-p
           #:make-paired-trial-result
           #:paired-trial-result-n
           #:paired-trial-result-baseline-runs
           #:paired-trial-result-candidate-runs
           #:paired-trial-result-wins
           #:paired-trial-result-ties
           #:paired-trial-result-losses
           #:paired-trial-result-delta
           #:paired-trial-result-confidence
           #:run-paired-trials
           #:paired-trial-gate-passes-p
           #:assert-paired-trial-promote

           #:promotion-stage-p
           #:promotion-stages
           #:next-promotion-stage
           #:promotion-record
           #:promotion-record-p
           #:make-promotion-record
           #:promotion-record-stage
           #:promotion-record-rollback-p
           #:promotion-record-reason
           #:promotion-record-from-stage
           #:make-rollback-marker
           #:rollback-marker-p

           #:llm-judge-scorer
           #:make-llm-judge-scorer
           #:llm-judge-scorer-backend
           #:llm-judge-scorer-prompt))

(in-package #:eval-protocol)
