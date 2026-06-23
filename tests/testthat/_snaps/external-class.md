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
      ! Can't find external class <not_a_pkg::X>:
      * Package 'not_a_pkg' is not installed.
    Code
      resolve_external_class_req(new_external_class("too.old", "X", "2.0.0"))
    Condition
      Error:
      ! Can't find external class <too.old::X>:
      * Package 'too.old' needs version 2.0.0, but only 1.0.0 is available.
    Code
      resolve_external_class_req(new_external_class("too.old", "X"))
    Condition
      Error:
      ! Package 'too.old' must bind `X` to the S7 class <too.old::X>.

# external class works as a property type for self-reference

    Code
      Tree(label = "bad", child = 1)
    Condition
      Error in `Tree()`:
      ! <mypkg::Tree> object properties are invalid:
      - @child must be <NULL> or <mypkg::Tree>, not <double>

# external class property validation reports validator errors

    Code
      Holder(child = invalid)
    Condition
      Error in `Holder()`:
      ! <S7::Holder> object properties are invalid:
      - @child: x must be non-negative

