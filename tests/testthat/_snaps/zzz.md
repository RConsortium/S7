# S7_class validates its underlying data

    <X> object is invalid:
    - Underlying data is corrupt

# $ gives useful error

    Code
      x$y
    Condition
      Error:
      ! Can't get S7 properties with `$`. Did you mean `x@y`?
    Code
      x$y <- 1
    Condition
      Error:
      ! Can't set S7 properties with `$`. Did you mean `...@y <- 1`?

# [ gives more accurate error

    Code
      x <- new_class("foo")()
      x[1]
    Condition
      Error in `check_subsettable()`:
      ! S7 objects are not subsettable.
    Code
      x[1] <- 1
    Condition
      Error in `check_subsettable()`:
      ! S7 objects are not subsettable.

# [[ gives more accurate error

    Code
      x <- new_class("foo")()
      x[[1]]
    Condition
      Error in `check_subsettable()`:
      ! S7 objects are not subsettable.
    Code
      x[[1]] <- 1
    Condition
      Error in `check_subsettable()`:
      ! S7 objects are not subsettable.

