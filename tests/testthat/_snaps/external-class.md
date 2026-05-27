# new_external_class() validates inputs

    Code
      new_external_class(1, "x")
    Condition
      Error:
      ! `package` must be a string.
    Code
      new_external_class("pkg", 1)
    Condition
      Error:
      ! `name` must be a string.

# print method works

    Code
      print(new_external_class("foo", "Bar"))
    Output
      <S7_external_class> foo::Bar
    Code
      print(new_external_class("foo", "Bar", version = "1.0"))
    Output
      <S7_external_class> foo::Bar (>= 1.0)

# external class works as a property type for self-reference

    Code
      tree(label = "bad", child = 1)
    Condition
      Error:
      ! <mypkg::tree> object properties are invalid:
      - @child must be <NULL> or <mypkg::tree>, not <double>

