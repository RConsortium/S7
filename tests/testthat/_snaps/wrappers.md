# S7 objects error when used with common generics

    Code
      foo <- new_class("foo")
      mean(foo())
    Error <simpleError>
      mean() is not implemented for S7 object <foo>.
    Code
      sum(foo())
    Error <simpleError>
      sum() is not implemented for S7 object <foo>.
    Code
      sin(foo())
    Error <simpleError>
      sin() is not implemented for S7 object <foo>.
    Code
      Re(foo())
    Error <simpleError>
      Re() is not implemented for S7 object <foo>.

