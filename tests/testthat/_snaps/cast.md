# can register cast methods

    Code
      cast(obj, double)
    Error <simpleError>
      Can't find method for generic `cast()` with dispatch classes:
      - from: casttest
      - to  : double

# cast_next(): checks to

    Code
      cast_next(R7_object)
    Error <simpleError>
      Can't cast: R7_object has no parent class
    Code
      foo <- new_class("foo")
      cast_next(foo(), character)
    Error <simpleError>
      Can't cast: <foo> doesn't inherit from <character>

