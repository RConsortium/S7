# has useful print method

    Code
      foo1 := new_class(package = NULL)
      foo2 := new_class(package = NULL)
      new_union(foo1, foo2)
    Output
      <S7_union>: <foo1> or <foo2>

# base unions display as expected

    Code
      class_vector
    Output
      <S7_union>: <logical>, <integer>, <double>, <complex>, <character>, <raw>, <expression>, or <list>
    Code
      str(class_vector)
    Output
      <S7_union>: <logical>, <integer>, <double>, <complex>, <character>, <raw>, <expression>, or <list>

