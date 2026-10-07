# Project Files

| Path | Contents |
|---|---|
| `haskell/Expr.hs` | Expression types, `eval`, `simplify` and the list functions |
| `haskell/Samples.hs` | Sample expressions with expected results |
| `haskell/Main.hs` | Demo program |
| `haskell/Tests.hs` | Unit tests |
| `haskell/Props.hs` | QuickCheck properties |
| `haskell/TypedExpr.hs` | GADT version of the language |
| `haskell/fp-project.cabal` | Cabal package |
| `haskell/sample_output.txt` | Output of `Main.hs` |
| `haskell/props_output.txt` | Output of the QuickCheck properties |
| `haskell/compile_errors.txt` | GHC errors for invalid expressions |
| `imperative/imperative_eval.py` | Imperative Python version of `eval` |
| `imperative/imperative_output.txt` | Output of the Python version |
| `report/report.pdf` | Report |
| `report/main.tex` | LaTeX source of the report |

## Running

Needs GHC (tested with 9.6.7), cabal for `cabal test`, and Python 3.10 or newer.

```bash
cd haskell
runghc Main.hs
runghc Tests.hs
cabal test
cd ../imperative
python imperative_eval.py
```

## Building the report

```bash
cd report
latexmk -pdf -jobname=report main.tex
```
