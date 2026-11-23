# can register convert methods

    Code
      convert(obj, to = class_double)
    Condition
      Error in `as.double()`:
      ! cannot coerce type 'object' to vector of type 'double'

# downcasts only splice plain lists of property values

    Code
      convert(1L, Box, df)
    Condition
      Error in `convert()`:
      ! All arguments to `...` must be named.

# downcasts preserve external-pointer sources

    Code
      S7_data(parent)
    Condition
      Error in `S7_data()`:
      ! Can't call `S7_data()` on <externalptr> objects with reference semantics.
    Code
      S7_data(parent) <- fresh_pointer()
    Condition
      Error in `S7_data<-`:
      ! Can't call `S7_data<-` on <externalptr> objects with reference semantics.
    Code
      convert(child, Handle)
    Condition
      Error in `convert()`:
      ! Can't call `convert()` on <externalptr> objects with reference semantics.

# downcasts start with fresh accessor and validation state

    Code
      valid_eventually(parent, function(x) {
        convert(x, Invalid)
        x
      })
    Condition
      Error:
      ! <S7::Invalid> object is invalid:
      - invalid child

# fallback convert rejects sibling S7 objects with additional S3 classes

    Code
      convert(child, to = Sibling)
    Condition
      Error in `convert()`:
      ! Can't find method with dispatch classes:
      - from: <Child>
      - to  : <Sibling>

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

