# validation uses typeof

    Code
      class_integer$validator(TRUE)
    Output
      [1] "Underlying data must be <integer> not <logical>"

# base class display as expected

    Code
      class_integer
    Output
      <S7_base_class>: <integer>
    Code
      str(class_integer)
    Output
      <S7_base_class>: <integer>

