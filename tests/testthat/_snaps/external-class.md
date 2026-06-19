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

# external class resolution explains class binding contract

    Code
      new_class(name = "Holder", properties = list(child = Bar))
    Condition
      Error:
      ! Can't find external class <dep::Bar>:
      * Package 'dep' must bind an S7 class to `Bar` with @name 'Bar' and @package 'dep'.

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

# versioned external class checks package version

    Code
      S7_inherits(versioned_pkg$Foo(), Foo)
    Condition
      Error:
      ! Can't find external class <versioned_pkg::Foo>:
      * Package 'versioned_pkg' needs version 999.0, but only 0.0.0 is available.

