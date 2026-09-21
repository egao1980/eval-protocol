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

## Calibration (0.3.0)

Calibration is empirical for `(backend, model-version, cohort, window)`. It is not a type.

Proper scores `brier-score` / `log-loss` consume a probability **mass** alist and a true outcome (binary `T`/`NIL` also match `:true`/`:false`). `reliability-bins` and `expected-calibration-error` take `(predicted-p-of-chosen . correct-p)` pairs — equal-width bins on `[0,1]`, with `1.0` in the last bin. `automation-at-budget` is the fraction of cases that can be auto-acted when we only act at `p_chosen >= t` and the error among acted cases stays `<= max-error` (lowest covering `t` that meets the budget; if several observed `t` induce that set, the highest is the cutoff).

`option-order-spread` and `isolation-delta` measure permute / packed-vs-separate stability. `make-drift-report` / `drift-report` compare Brier and ECE deltas vs a pinned baseline cohort.

Gates (both implement `gate-passes-p`):

- `calibration-gate` — candidate Brier and ECE must not worsen vs baseline by more than `:brier-delta` / `:ece-delta` (default 0)
- `option-order-gate` — max spread across options `<= :max-spread`

Jev/Kev `confidence` is `choice-concentration` — `(pmax - 1/k) / (1 - 1/k)` — a **display** helper, never `P(correct)`. Gates read mass / Brier / ECE only (`concentration-as-accuracy-forbidden`).

## License

MIT
