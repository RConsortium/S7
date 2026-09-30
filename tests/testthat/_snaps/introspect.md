# S7_methods() prints the signature column readably

    Code
      print(S7_methods(generic = gen))
    Output
        generic package signature
      1     gen    <NA>     <Foo>
      2     gen    <NA>     <Bar>

# S7_methods() validates inputs

    Code
      S7_methods(generic = "not a generic")
    Condition
      Error in `S7_methods()`:
      ! `generic` must be an S7 generic.

