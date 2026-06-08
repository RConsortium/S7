# can register convert methods

    Code
      convert(obj, to = class_double)
    Condition
      Error in `as.double()`:
      ! cannot coerce type 'object' to vector of type 'double'

# fallback convert / errors if single unnamed list has unnamed elements (#497)

    Code
      convert(Foo(x = 1), Bar, list(2))
    Condition
      Error in `convert()`:
      ! All elements of `..1` must be named.

# convert() errors when upcasting to an abstract class (#680)

    Code
      convert(Bar(), Foo)
    Condition
      Error in `convert()`:
      ! Can't convert to abstract class <Foo>.

