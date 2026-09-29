# helpers reject unknown skip values

    Code
      S7_generic_call(skip = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "none", "super"
    Code
      S7_user_frame(skip = "invalid")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "none", "super"

# helpers error when called outside a method

    Code
      S7_generic_call()
    Condition
      Error in `S7_generic_call()`:
      ! Must be called from within a method.
    Code
      S7_user_frame()
    Condition
      Error in `S7_user_frame()`:
      ! Must be called from within a method.
    Code
      S7_generic_fun()
    Condition
      Error in `S7_generic_fun()`:
      ! Must be called from within a method.

# helpers error from generic bodies outside active methods

    Code
      before(1)
    Condition
      Error in `S7_generic_call()`:
      ! Must be called from within a method.

---

    Code
      after(1)
    Condition
      Error in `S7_user_frame()`:
      ! Must be called from within a method.

# helpers error in methods not dispatched by an S7 generic

    Code
      outer(1)
    Condition
      Error in `S7_generic_call()`:
      ! Must be called from within a method.

# helpers error while forcing dispatch arguments

    Code
      foo({
        S7_generic_call()
        1
      })
    Condition
      Error in `S7_generic_call()`:
      ! Must be called from within a method.

