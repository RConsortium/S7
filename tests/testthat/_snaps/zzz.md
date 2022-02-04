# has useful print method

    Code
      foo1 <- new_class("foo1")
      foo2 <- new_class("foo2")
      new_union(foo1, foo2)
    Output
      <R7_union>: <foo1> or <foo2>

# base unions print as expected

    Code
      base_classes$numeric
    Output
      <R7_union>: <integer> or <double>
    Code
      base_classes$atomic
    Output
      <R7_union>: <logical>, <integer>, <double>, <complex>, <character>, or <raw>
    Code
      base_classes$vector
    Output
      <R7_union>: <logical>, <integer>, <double>, <complex>, <character>, <raw>, <expression>, or <list>

