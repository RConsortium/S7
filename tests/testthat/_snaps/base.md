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

# matrix/array constructors enforce element type

    Code
      Mat(1:4, nrow = 2)
    Condition
      Error in `Mat()`:
      ! <S7::Mat> object is invalid:
      - Underlying data must be <double> not <integer>

# class_matrix and class_array can't be a parent

    Code
      new_class("Foo", parent = class_matrix)
    Condition
      Error in `new_class()`:
      ! `parent` must be an S7 class, S3 class, or base type, not an S7 union.
    Code
      new_class("Foo", parent = class_array)
    Condition
      Error in `new_class()`:
      ! `parent` must be an S7 class, S3 class, or base type, not an S7 union.

# element-typed properties enforce element type

    Code
      Foo(x = matrix(1:4, 2))
    Condition
      Error in `Foo()`:
      ! <S7::Foo> object properties are invalid:
      - @x Underlying data must be <double> not <integer>

