# eval-protocol

Lispy **CLOS** evaluation API for [cl-stack](https://github.com/egao1980/cl-stack) — versioned datasets, scorers, runs, promotion gates.

| System | Role | Repo |
|--------|------|------|
| `eval-protocol` (`stack-eval`) | Protocol / API (LLM-free) | this repo |
| `eval-protocol/judge` | `llm-judge-scorer` via `llm-protocol` | this repo |

```lisp
(asdf:load-system "eval-protocol")

(let* ((ds (stack-eval:make-eval-dataset
            :name "smoke"
            :cases (list (stack-eval:make-eval-case :input 2 :expected 4))))
       (run (stack-eval:run-eval ds (lambda (x) (* x 2)))))
  (stack-eval:eval-report run))
```

Scorer or target failure → `scorer-error` (`:cause`) with restarts `skip-case`, `retry-case`, `score-as-failure`.

## Roles, trials, promotion (0.2.0)

Datasets and cases carry a role (`:train` `:dev` `:holdout`) plus lineage (`source`, `role`, parent dataset version). `add-case` of `:human-feedback` / `:production` cannot enter a `:holdout` split (`holdout-admission-error`; restart `force-holdout-admission` is explicit and named, default deny). `(dataset-split dataset :role :holdout)` and `(assert-no-holdout-overlap train holdout)` keep search/train data off the promotion holdout.

`run-paired-trials` runs N paired baseline/candidate evals; `paired-trial-gate-passes-p` / `assert-paired-trial-promote` refuse promote when `n` < min-sample or sign-test confidence is below the threshold.

Promotion stages are data: `:shadow` → `:canary` → `:promote`, plus `make-rollback-marker`.

## License

MIT
