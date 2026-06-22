# super() checks to

    Code
      foo := new_class(package = NULL)
      super(foo(), class_character)
    Condition
      Error in `super()`:
      ! <foo> doesn't inherit from <character>.
    Code
      super(foo(), class_numeric)
    Condition
      Error in `super()`:
      ! `to` must be an S7, S3, S4, or base class, not an S7 union.
    Code
      super(foo(), NULL)
    Condition
      Error in `super()`:
      ! `to` must be an S7, S3, S4, or base class, not NULL.

# super() displays nicely

    Code
      f1 <- super(foo2(), foo1)
      f1
    Output
      super(<foo2>, <foo1>)
    Code
      str(list(f1))
    Output
      List of 1
       $ : super(<foo2>, <foo1>)

