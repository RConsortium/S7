# := validates its inputs

    Code
      "foo" := new_thing()
    Condition
      Error in `"foo" := new_thing()`:
      ! Left-hand side of `:=` must be a symbol.
    Code
      foo := 10
    Condition
      Error in `foo := 10`:
      ! Right-hand side of `:=` must be a function call.
    Code
      foo := new_thing(name = "bar")
    Condition
      Error in `foo := new_thing(name = "bar")`:
      ! Right-hand side of `:=` must not already supply a `name` argument.
    Code
      foo := no_name()
    Condition
      Error in `no_name()`:
      ! unused argument (name = "foo")

