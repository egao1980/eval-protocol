(in-package #:eval-protocol/tests)

(defun %case (input expected &key role source parent-version metadata)
  (eval-protocol:make-eval-case
   :input input :expected expected
   :role role :source source :parent-version parent-version
   :metadata metadata))

(deftest dataset-roles-and-lineage
  (let* ((c1 (%case "a" "a" :role :train))
         (c2 (%case "b" "b" :role :dev))
         (c3 (%case "c" "c" :role :holdout))
         (ds (eval-protocol:make-eval-dataset
              :name "suite"
              :cases (list c1 c2 c3)))
         (added (eval-protocol:add-case
                 ds (%case "d" "d") :source :curated :role :train))
         (new (car (last (eval-protocol:eval-dataset-cases added)))))
    (ok (eval-protocol:dataset-role-p :train))
    (ok (eval-protocol:dataset-role-p :dev))
    (ok (eval-protocol:dataset-role-p :holdout))
    (ng (eval-protocol:dataset-role-p :test))
    (ok (eq :train (eval-protocol:eval-case-role
                    (first (eval-protocol:eval-dataset-cases ds)))))
    (ok (eq :dev (eval-protocol:eval-case-role
                  (second (eval-protocol:eval-dataset-cases ds)))))
    (ok (eq :holdout (eval-protocol:eval-case-role
                      (third (eval-protocol:eval-dataset-cases ds)))))
    (ok (eq :train (eval-protocol:eval-case-role new)))
    (ok (eq :curated (eval-protocol:eval-case-source new)))
    (ok (equal (eval-protocol:eval-dataset-version ds)
               (eval-protocol:eval-case-parent-version new)))
    (ok (equal (list :source :curated
                     :role :train
                     :parent-version (eval-protocol:eval-dataset-version ds))
               (eval-protocol:eval-case-lineage new)))))

(deftest-parametrize human-feedback-cannot-enter-holdout
    ((source)
     :ids ("human-feedback" "production")
     (:human-feedback)
     (:production))
  (let* ((holdout (eval-protocol:make-eval-dataset
                   :name "hold"
                   :role :holdout
                   :cases (list (%case "gold" "gold"))))
         (case (%case "fb" "fb")))
    (ok (signals (eval-protocol:add-case holdout case :source source)
                 'eval-protocol:holdout-admission-error))
    (ok (signals (eval-protocol:add-case
                  (eval-protocol:make-eval-dataset :name "train" :role :train)
                  case :source source :role :holdout)
                 'eval-protocol:holdout-admission-error))
    (ok (= 1 (length (eval-protocol:eval-dataset-cases holdout))))))

(deftest human-feedback-may-enter-train
  (let* ((train (eval-protocol:make-eval-dataset
                 :name "train" :role :train
                 :cases (list (%case "a" "a"))))
         (holdout-ds (eval-protocol:make-eval-dataset
                      :name "mixed"
                      :cases (list (%case "h" "h" :role :holdout))))
         (into-train (eval-protocol:add-case
                      train (%case "fb" "fb") :source :human-feedback))
         (explicit-train (eval-protocol:add-case
                          holdout-ds (%case "fb2" "fb2")
                          :source :human-feedback :role :train))
         (landed (car (last (eval-protocol:eval-dataset-cases into-train)))))
    (ok (eq :train (eval-protocol:eval-case-role landed)))
    (ok (eq :human-feedback (eval-protocol:eval-case-source landed)))
    (ok (eq :train (eval-protocol:eval-case-role
                    (car (last (eval-protocol:eval-dataset-cases explicit-train))))))))

(deftest force-holdout-admission-restart
  (let* ((holdout (eval-protocol:make-eval-dataset
                   :name "hold" :role :holdout
                   :cases (list (%case "gold" "gold"))))
         (forced (handler-bind
                     ((eval-protocol:holdout-admission-error
                       (lambda (c)
                         (declare (ignore c))
                         (invoke-restart 'eval-protocol:force-holdout-admission))))
                   (eval-protocol:add-case
                    holdout (%case "fb" "fb") :source :human-feedback)))
         (landed (car (last (eval-protocol:eval-dataset-cases forced)))))
    (ok (eq :holdout (eval-protocol:eval-case-role landed)))
    (ok (eq :human-feedback (eval-protocol:eval-case-source landed)))
    (ok (equal (eval-protocol:eval-dataset-version holdout)
               (eval-protocol:eval-case-parent-version landed)))
    (ok (= 2 (length (eval-protocol:eval-dataset-cases forced))))))

(deftest dump-load-preserves-roles-and-lineage
  (let* ((base (eval-protocol:make-eval-dataset
                :name "round" :role :train
                :cases (list (%case 1 1 :role :train))))
         (ds (eval-protocol:add-case
              base (%case 2 2) :source :human-feedback :role :dev))
         (text (eval-protocol:dump-dataset ds))
         (loaded (eval-protocol:load-dataset text))
         (c1 (first (eval-protocol:eval-dataset-cases loaded)))
         (c2 (second (eval-protocol:eval-dataset-cases loaded))))
    (ok (stringp text))
    (ok (eq :train (eval-protocol:eval-dataset-role loaded)))
    (ok (equal (eval-protocol:eval-dataset-version ds)
               (eval-protocol:eval-dataset-version loaded)))
    (ok (eq :train (eval-protocol:eval-case-role c1)))
    (ok (eq :dev (eval-protocol:eval-case-role c2)))
    (ok (eq :human-feedback (eval-protocol:eval-case-source c2)))
    (ok (equal (eval-protocol:eval-dataset-version base)
               (eval-protocol:eval-case-parent-version c2)))
    (ok (equal (eval-protocol:eval-case-lineage
                (second (eval-protocol:eval-dataset-cases ds)))
               (eval-protocol:eval-case-lineage c2)))))

(deftest dataset-split-and-no-holdout-overlap
  (let* ((ds (eval-protocol:make-eval-dataset
              :name "all"
              :cases (list (%case "t" "t" :role :train)
                           (%case "d" "d" :role :dev)
                           (%case "h" "h" :role :holdout))))
         (train (eval-protocol:dataset-split ds :role :train))
         (dev (eval-protocol:dataset-split ds :role :dev))
         (holdout (eval-protocol:dataset-split ds :role :holdout))
         (overlap-train (eval-protocol:make-eval-dataset
                         :name "bad-train"
                         :role :train
                         :cases (list (%case "h" "h" :role :train)))))
    (ok (eq :train (eval-protocol:eval-dataset-role train)))
    (ok (eq :dev (eval-protocol:eval-dataset-role dev)))
    (ok (eq :holdout (eval-protocol:eval-dataset-role holdout)))
    (ok (= 1 (length (eval-protocol:eval-dataset-cases train))))
    (ok (= 1 (length (eval-protocol:eval-dataset-cases holdout))))
    (ok (eval-protocol:assert-no-holdout-overlap train holdout))
    (ok (signals (eval-protocol:assert-no-holdout-overlap overlap-train holdout)
                 'eval-protocol:holdout-overlap-error))))

(deftest paired-trials-and-promote-gate
  (let* ((ds (eval-protocol:make-eval-dataset
              :name "pairs"
              :role :holdout
              :cases (list (%case 1 2) (%case 3 6))))
         (win (eval-protocol:run-paired-trials
               ds (lambda (x) x) (lambda (x) (* x 2)) :n 5))
         (tie (eval-protocol:run-paired-trials
               ds (lambda (x) (* x 2)) (lambda (x) (* x 2)) :n 3))
         (short (eval-protocol:make-paired-trial-result
                 :n 2 :wins 2 :losses 0 :confidence 3/4)))
    (ok (eval-protocol:paired-trial-result-p win))
    (ok (= 5 (eval-protocol:paired-trial-result-n win)))
    (ok (= 5 (eval-protocol:paired-trial-result-wins win)))
    (ok (zerop (eval-protocol:paired-trial-result-losses win)))
    (ok (> (eval-protocol:paired-trial-result-confidence win) 9/10))
    (ok (eval-protocol:paired-trial-gate-passes-p
         win :min-sample 5 :confidence-threshold 9/10))
    (ok (eval-protocol:assert-paired-trial-promote
         win :min-sample 5 :confidence-threshold 9/10))
    (ng (eval-protocol:paired-trial-gate-passes-p
         win :min-sample 10 :confidence-threshold 1/2))
    (ng (eval-protocol:paired-trial-gate-passes-p
         tie :min-sample 3 :confidence-threshold 1/2))
    (ok (signals (eval-protocol:assert-paired-trial-promote
                  short :min-sample 5 :confidence-threshold 9/10)
                 'eval-protocol:paired-trial-gate-error))
    (ok (signals (eval-protocol:assert-paired-trial-promote
                  tie :min-sample 3 :confidence-threshold 9/10)
                 'eval-protocol:paired-trial-gate-error))))

(deftest promotion-stages-and-rollback-marker
  (ok (equal '(:shadow :canary :promote) (eval-protocol:promotion-stages)))
  (ok (eval-protocol:promotion-stage-p :shadow))
  (ok (eval-protocol:promotion-stage-p :canary))
  (ok (eval-protocol:promotion-stage-p :promote))
  (ng (eval-protocol:promotion-stage-p :rollback))
  (ok (eq :canary (eval-protocol:next-promotion-stage :shadow)))
  (ok (eq :promote (eval-protocol:next-promotion-stage :canary)))
  (ok (null (eval-protocol:next-promotion-stage :promote)))
  (let ((shadow (eval-protocol:make-promotion-record :shadow))
        (marker (eval-protocol:make-rollback-marker
                 :from :canary :reason "gate failed")))
    (ok (eval-protocol:promotion-record-p shadow))
    (ok (eq :shadow (eval-protocol:promotion-record-stage shadow)))
    (ng (eval-protocol:promotion-record-rollback-p shadow))
    (ok (eval-protocol:rollback-marker-p marker))
    (ok (eq :canary (eval-protocol:promotion-record-stage marker)))
    (ok (eq :canary (eval-protocol:promotion-record-from-stage marker)))
    (ok (equal "gate failed" (eval-protocol:promotion-record-reason marker)))
    (ok (signals (eval-protocol:make-promotion-record :nope)
                 'eval-protocol:eval-error))))
