# can register convert methods

    Code
      convert(obj, to = class_double)
    Condition
      Error in `as.double()`:
      ! cannot coerce type 'object' to vector of type 'double'

# fallback convert rejects unrelated S4 name matches for S7 targets

    Code
      convert(from, to = ConvertS4NameCollisionTo)
    Condition
      Error in `convert()`:
      ! Can't find method with dispatch classes:
      - from: S4<ConvertS4NameCollisionFrom>
      - to  : <ConvertS4NameCollisionTo>

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

