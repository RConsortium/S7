# R7_class validates its underlying data

    <X> object is invalid:
    - Underlying data is corrupt

# $ gives useful error

    Code
      x$y
    Error <simpleError>
      Can't get R7 properties with `$`. Did you mean `x@y`?
    Code
      x$y <- 1
    Error <simpleError>
      Can't set R7 properties with `$`. Did you mean `...@y <- 1`?

# [ gives more accurate error

    Code
      x <- new_class("foo")()
      x[1]
    Error <simpleError>
      R7 objects are not subsettable.
    Code
      x[1] <- 1
    Error <simpleError>
      R7 objects are not subsettable.

# [[ gives more accurate error

    Code
      x <- new_class("foo")()
      x[[1]]
    Error <simpleError>
      R7 objects are not subsettable.
    Code
      x[[1]] <- 1
    Error <simpleError>
      R7 objects are not subsettable.

# check_subsettable

    Code
      x = new_class("foo")()
      check_subsettable(x)
    Error <simpleError>
      R7 objects are not subsettable.
    Code
      check_subsettable(x, allow_env = TRUE)
    Error <simpleError>
      R7 objects are not subsettable.

---

    Code
      check_subsettable(x)
    Error <simpleError>
      R7 objects are not subsettable.

