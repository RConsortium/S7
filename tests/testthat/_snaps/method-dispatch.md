# generics pass ... to methods

    unused argument (z = 2)

# single dispatch fails with informative messages

    Code
      fail(TRUE)
    Error <simpleError>
      Can't find method for `fail(<logical>)`.
    Code
      fail(tibble::tibble())
    Error <simpleError>
      Can't find method for `fail(S3<tbl_df/tbl/data.frame>)`.
    Code
      fail(foo())
    Error <simpleError>
      Can't find method for `fail(<foo>)`.
    Code
      fail(Foo(x = 1))
    Error <simpleError>
      Can't find method for `fail(S4<Foo>)`.

# multiple dispatch fails with informative messages

    Code
      fail(TRUE)
    Error <simpleError>
      Can't find method for generic `fail(x, y)`:
      - x: <logical>
      - y: MISSING
    Code
      fail(, TRUE)
    Error <simpleError>
      Can't find method for generic `fail(x, y)`:
      - x: MISSING
      - y: <logical>
    Code
      fail(TRUE, TRUE)
    Error <simpleError>
      Can't find method for generic `fail(x, y)`:
      - x: <logical>
      - y: <logical>

