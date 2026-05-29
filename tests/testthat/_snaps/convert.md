# can register convert methods

    Code
      convert(obj, to = class_double)
    Condition
      Error in `convert()`:
      ! Can't find method with dispatch classes:
      - from: <converttest>
      - to  : <double>

# fallback convert / errors if single unnamed list has unnamed elements (#497)

    Code
      convert(Foo(x = 1), Bar, list(2))
    Condition
      Error in `convert()`:
      ! All elements of `..1` must be named.

