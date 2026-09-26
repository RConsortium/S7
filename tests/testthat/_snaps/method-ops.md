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

# `!` requires a length-1 signature

    Code
      method(`!`, list(Logical, class_missing)) <- (function(e1, e2) e1)
    Condition
      Error in `method<-`:
      ! `signature` must be length 1.

