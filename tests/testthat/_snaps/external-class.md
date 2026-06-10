# new_external_class() validates inputs

    Code
      new_external_class(1, "x")
    Condition
      Error in `new_external_class()`:
      ! `package` must be a string.
    Code
      new_external_class("pkg", 1)
    Condition
      Error in `new_external_class()`:
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

# resolve_external_class_req() errors per failure mode

    Code
      resolve_external_class_req(new_external_class("not_a_pkg", "X"))
    Condition
      Error:
      ! Can't find external class <not_a_pkg::X>: package 'not_a_pkg' is not installed.
    Code
      resolve_external_class_req(new_external_class("S7", "S7_object", "999.0"))
    Condition
      Error:
      ! Can't find external class <S7::S7_object>: package 'S7' is version 0.2.2.9000, but >= 999.0 is required.
    Code
      resolve_external_class_req(new_external_class("S7", "not_a_class"))
    Condition
      Error:
      ! Can't find external class <S7::not_a_class>: 'not_a_class' is not found in package 'S7'.

# external class works as a property type for self-reference

    Code
      Tree(label = "bad", child = 1)
    Condition
      Error in `Tree()`:
      ! <mypkg::Tree> object properties are invalid:
      - @child must be <NULL> or <mypkg::Tree>, not <double>

