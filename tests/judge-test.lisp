(in-package #:eval-protocol/tests)

(deftest llm-judge-mock-json
  (let* ((backend (llm-protocol:make-mock-llm-backend
                   :handler (lambda (b turns &key &allow-other-keys)
                              (declare (ignore b turns))
                              (llm-protocol:make-llm-response
                               :parts (list (llm-protocol:make-llm-text-part
                                             :text "{\"score\":1,\"verdict\":\"pass\",\"rationale\":\"ok\"}"))))))
         (scorer (eval-protocol:make-llm-judge-scorer :backend backend))
         (case (eval-protocol:make-eval-case :input "q" :expected "a"))
         (score (eval-protocol:score-case scorer case "a")))
    (ok (eval-protocol:eval-score-p score))
    (ok (= 1 (eval-protocol:eval-score-value score)))
    (ok (eq :pass (eval-protocol:eval-score-verdict score)))
    (ok (equal "ok" (eval-protocol:eval-score-rationale score)))))
