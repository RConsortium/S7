# can register convert methods

    Code
      convert(obj, to = class_double)
    Condition
      Error in `as.double()`:
      ! cannot coerce type 'object' to vector of type 'double'

# convert() errors when upcasting to an abstract class (#680)

    Code
      convert(Bar(), Foo)
    Condition
      Error in `convert()`:
      ! Can't convert to abstract class <Foo>.

# convert() errors when upcasting to an abstract S3 class (#686)

    Code
      convert(.POSIXct(1), class_POSIXt)
    Condition
      Error in `convert()`:
      ! Can't convert to abstract class <POSIXt>.
