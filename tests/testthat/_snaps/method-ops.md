# Ops bridges preserve a class's existing base methods (#544)

    Code
      factor("a") + 1
    Condition
      Warning in `method()`:
      '+' not meaningful for factors
    Output
      [1] NA

