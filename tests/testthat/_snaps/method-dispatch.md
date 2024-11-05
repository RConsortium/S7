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

# errors from dispatched methods have reasonable tracebacks

    Code
      my_generic(10)
    Output
      [[1]]
      my_generic(10)
      
      [[2]]
      S7::S7_dispatch()
      
      [[3]]
      `method(my_generic, class_double)`(x = 10, ...)
      

---

    Code
      my_generic(3, 4)
    Output
      [[1]]
      my_generic(3, 4)
      
      [[2]]
      S7::S7_dispatch()
      
      [[3]]
      `method(my_generic, list(class_double, class_double))`(x = 3, 
          y = 4, ...)
      

