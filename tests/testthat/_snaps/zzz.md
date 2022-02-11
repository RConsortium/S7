# has useful print method

    Code
      foo1 <- new_class("foo1")
      foo2 <- new_class("foo2")
      new_union(foo1, foo2)
    Output
      <R7_union>: <foo1> or <foo2>

# base unions print as expected

    Code
      base_unions
    Output
      $numeric
      <R7_union>: <integer> or <double>
      
      $atomic
      <R7_union>: <logical>, <integer>, <double>, <complex>, <character>, or <raw>
      
      $vector
      <R7_union>: <logical>, <integer>, <double>, <complex>, <character>, <raw>, <expression>, or <list>
      

# base classes types check their data

    Code
      base_classes$integer(TRUE)
    Error <simpleError>
      <integer> object is invalid:
      - Underlying data must be <integer> not <logical>

