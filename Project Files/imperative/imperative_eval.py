# Imperative version of eval for comparison. Run: python imperative_eval.py
# Achintha H.G.R. (EG/2021/4384)
# Expressions are tuples tagged with a string, e.g. ("add", ("var", "x"), ("lit", 3))


class EvalError(Exception):
    pass


def evaluate(env, expr):
    result = None
    match expr[0]:
        case "lit":
            result = expr[1]
        case "var":
            if expr[1] not in env:
                raise EvalError("undefined variable: " + expr[1])
            result = env[expr[1]]
        case "add":
            result = evaluate(env, expr[1]) + evaluate(env, expr[2])
        case "sub":
            result = evaluate(env, expr[1]) - evaluate(env, expr[2])
        case "mul":
            result = evaluate(env, expr[1]) * evaluate(env, expr[2])
        case "div":
            left = evaluate(env, expr[1])
            right = evaluate(env, expr[2])
            if right == 0:
                raise EvalError("division by zero")
            result = left / right
        case "let":
            name = expr[1]
            had_old, old = name in env, env.get(name)
            env[name] = evaluate(env, expr[2])
            try:
                result = evaluate(env, expr[3])
            finally:
                if had_old:
                    env[name] = old
                else:
                    del env[name]
        case "if":
            if evaluate(env, expr[1]):
                result = evaluate(env, expr[2])
            else:
                result = evaluate(env, expr[3])
        case "lt":
            result = evaluate(env, expr[1]) < evaluate(env, expr[2])
        case "eq":
            result = evaluate(env, expr[1]) == evaluate(env, expr[2])
        case "and":
            result = evaluate(env, expr[1]) and evaluate(env, expr[2])
        case "not":
            result = not evaluate(env, expr[1])
        case _:
            raise EvalError("unknown expression tag: " + str(expr[0]))
    return result


def run(env, expr):
    try:
        return evaluate(env, expr)
    except EvalError as err:
        return "error: " + str(err)


x, y, z, w = ("var", "x"), ("var", "y"), ("var", "z"), ("var", "w")
a = ("var", "a")


def lit(n):
    return ("lit", n)


SAMPLES = [
    ("precedence", ("mul", ("add", x, lit(3)), y), 16),
    ("division", ("div", x, y), 2.5),
    ("let binding", ("let", "a", ("mul", x, lit(2)), ("add", a, lit(1))), 11),
    ("let shadowing",
     ("let", "x", lit(1), ("add", ("let", "x", lit(10), x), x)), 11),
    ("if / boolean ops",
     ("if", ("and", ("lt", x, lit(10)), ("not", ("eq", y, lit(0)))),
      ("div", x, y), lit(0)), 2.5),
    ("guarded division", ("if", ("eq", z, lit(0)), lit(0), ("div", x, z)), 0),
    ("division by zero", ("div", x, ("sub", y, lit(2))),
     "error: division by zero"),
    ("undefined variable", ("add", x, w), "error: undefined variable: w"),
    ("error inside let",
     ("let", "a", ("div", lit(1), z), ("add", a, lit(1))),
     "error: division by zero"),
]

# programs a type checker would reject
RUNTIME_ONLY_BUGS = [
    ("mistyped tag in a branch that does not run",
     ("if", ("lt", x, lit(10)), lit(1), ("mull", x, y))),
    ("mistyped tag in a branch that runs",
     ("if", ("lt", y, lit(1)), lit(1), ("mull", x, y))),
    ("a number used as a condition", ("if", x, lit(1), lit(2))),
    ("a boolean used as a number", ("add", ("lt", y, x), lit(3))),
    ("a string literal used as a number", ("add", lit("2"), lit(3))),
]


def main():
    env = {"x": 5, "y": 2, "z": 0}
    original = dict(env)
    print("Imperative (Python) evaluator - environment:", env)

    print("\n== The nine samples (expected vs actual) ==")
    for i, (name, expr, expected) in enumerate(SAMPLES, start=1):
        actual = run(env, expr)
        verdict = "PASS" if actual == expected else "FAIL"
        print(f"  [{i}] {name:<20} expected {str(expected):<34}"
              f" actual {str(actual):<34} {verdict}")
    print("  env restored after every let:", env == original)

    print("\n== Bugs that only show up (or never show up) at run time ==")
    for name, expr in RUNTIME_ONLY_BUGS:
        try:
            outcome = repr(run(env, expr))
        except Exception as err:
            outcome = f"crash: {type(err).__name__}: {err}"
        print(f"  {name:<45} -> {outcome}")


if __name__ == "__main__":
    main()
