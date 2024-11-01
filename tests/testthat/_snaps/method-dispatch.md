# generics pass ... to methods

    unused argument (z = 2)

# single dispatch fails with informative messages

    Code
      fail(TRUE)
    Condition
      Error:
      ! Can't find method for `fail(<logical>)`.
    Code
      fail(tibble::tibble())
    Condition
      Error:
      ! Can't find method for `fail(S3<tbl_df/tbl/data.frame>)`.
    Code
      fail(foo())
    Condition
      Error:
      ! Can't find method for `fail(<foo>)`.
    Code
      fail(Foo(x = 1))
    Condition
      Error:
      ! Can't find method for `fail(S4<Foo>)`.

# multiple dispatch fails with informative messages

    Code
      fail(TRUE)
    Condition
      Error:
      ! Can't find method for generic `fail(x, y)`:
      - x: <logical>
      - y: MISSING
    Code
      fail(, TRUE)
    Condition
      Error:
      ! Can't find method for generic `fail(x, y)`:
      - x: MISSING
      - y: <logical>
    Code
      fail(TRUE, TRUE)
    Condition
      Error:
      ! Can't find method for generic `fail(x, y)`:
      - x: <logical>
      - y: <logical>

# method dispatch works for class_missing

    Code
      foo_wrapper()
    Condition
      Error in `foo_wrapper()`:
      ! argument "xx" is missing, with no default

