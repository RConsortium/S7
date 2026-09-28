# Ops methods propagate missing-method errors from their bodies

    Code
      +Number(1)
    Condition
      Error:
      ! Can't find method for `other(<S7::Number>)`.

---

    Code
      -Number(1)
    Condition
      Error:
      ! Can't find method for `other(<S7::Number>)`.

---

    Code
      Number(1) + 1
    Condition
      Error:
      ! Can't find method for `other(<S7::Number>)`.

---

    Code
      !Flag(TRUE)
    Condition
      Error:
      ! Can't find method for `other(<S7::Flag>)`.

# Ops methods propagate missing-method errors from the same operator

    Code
      -Number(1)
    Condition
      Error:
      ! Can't find method for generic `-(e1, e2)`:
      - e1: <S7::Other>
      - e2: <S7::Other>

# Ops methods have fixed argument lists

    Code
      method(`+`, list(Number, class_missing)) <- (function(e1, e2, ...) NULL)
    Condition
      Error in `method<-`:
      ! +() generic lacks `...` so method formals must match generic formals exactly.
      - generic formals: +(e1, e2)
      - method formals:  +(e1, e2, ...)

---

    Code
      method(`!`, Number) <- (function(e1, ...) NULL)
    Condition
      Error in `method<-`:
      ! !() generic lacks `...` so method formals must match generic formals exactly.
      - generic formals: !(e1)
      - method formals:  !(e1, ...)

# matrixOps methods have fixed argument lists

    Code
      method(`%*%`, list(Number, Number)) <- (function(x, y, ...) NULL)
    Condition
      Error in `method<-`:
      ! %*%() generic lacks `...` so method formals must match generic formals exactly.
      - generic formals: %*%(x, y)
      - method formals:  %*%(x, y, ...)

# `!` requires a length-1 signature

    Code
      method(`!`, list(Logical, class_missing)) <- (function(e1, e2) e1)
    Condition
      Error in `method<-`:
      ! `signature` must be length 1.

