# EC8206 Functional Programming – Project Assignment

**A type-safe arithmetic expression interpreter in Haskell**

Achintha H.G.R. – EG/2021/4384  
Department of Electrical and Information Engineering, University of Ruhuna

The interpreter supports numeric literals, variables, `+ - * /`, `let`
bindings and `if` expressions over a separate boolean sub-language. Evaluation
reports division by zero and undefined variables through `Either` instead of
exceptions; `simplify` rewrites algebraic identities; and batch functions use
`map`, `foldr`, `filter` and partial application.

All project files are in [`Project Files/`](Project%20Files/), including the
[report](Project%20Files/report/report.pdf).

## Quick start

```bash
cd "Project Files/haskell"
runghc Main.hs      # sample evaluations, simplify and batch demos
runghc Tests.hs     # 72 unit tests
cabal test          # unit tests + 8 QuickCheck properties
```
