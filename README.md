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

## License

MIT
