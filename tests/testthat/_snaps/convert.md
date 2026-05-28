# can register convert methods

    Code
      convert(obj, to = class_double)
    Condition
      Error in `convert()`:
      ! Can't find method with dispatch classes:
      - from: <converttest>
      - to  : <double>

# fallback convert / errors when upcasting an environment

    Code
      convert(Child(), Parent)
    Condition
      Error:
      ! Can't use the default `convert()` method to upcast an environment:
      - from: <Child>
      - to  : <Parent>
      See ?class_environment for details.

