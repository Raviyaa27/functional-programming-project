# Project Files

A small, type-safe arithmetic expression interpreter in Haskell
(EC8206 Functional Programming, project assignment).

| Path | Contents |
|---|---|
| `haskell/Expr.hs` | Parts A–C: the `Expr`/`BExpr` types, `eval`, `simplify` and the higher-order batch functions |
| `haskell/Samples.hs` | The nine sample evaluations used in the report, with expected results |
| `haskell/Main.hs` | Demo program: sample evaluations, simplification, batch report |
| `haskell/Tests.hs` | 72 expected-vs-actual unit tests (needs only GHC) |
| `haskell/Props.hs` | 8 QuickCheck properties, 1000 random expressions each |
| `haskell/TypedExpr.hs` | Extension: the same language as a GADT indexed by result type |
| `haskell/fp-project.cabal` | Cabal package: `expr-demo`, `unit-tests` and `props` |
| `haskell/sample_output.txt` | Captured output of `Main.hs` |
| `haskell/props_output.txt` | Captured output of the QuickCheck properties |
| `haskell/compile_errors.txt` | GHC's output for invalid expressions (used in Part E) |
| `imperative/imperative_eval.py` | Part D: imperative Python version of `eval` |
| `imperative/imperative_output.txt` | Captured output of the Python version |
| `report/report.pdf` | The written report (cover page + 4 pages) |
| `report/main.tex` | LaTeX source of the report |

## Running

Requirements: GHC (tested with 9.6.7), cabal (only for `cabal test`),
Python 3.10+ (for the `match` statement).

```bash
cd haskell
runghc Main.hs          # demo
runghc Tests.hs         # unit tests, no packages needed
cabal test              # unit tests + QuickCheck properties
cabal run expr-demo     # the demo, built with cabal
cd ../imperative
python imperative_eval.py
```

## Building the report

```bash
cd report
latexmk -pdf -jobname=report main.tex
```

The report pulls its code listings straight from `../haskell/Expr.hs` and
`../imperative/imperative_eval.py`, so the line numbers it quotes always match
the source.
