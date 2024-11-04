# displays nicely

    Code
      print(bar)
    Output
      <S7_external_generic> foo::bar(x)

# new_method works with both hard and soft dependencies

    Code
      args(Foo)
    Output
      function (bar = t0::`An S7 Class`()) 
      NULL
    Code
      args(t2::`An S7 Class 2`)
    Output
      function (bar = t0::`An S7 Class`()) 
      NULL
    Code
      args(t2:::`An Internal Class`)
    Output
      function (foo = t0::`An S7 Class`(), bar = `An S7 Class 2`()) 
      NULL

---

    Code
      new_class("Foo", properties = list(bar = new_class("Made Up Class", package = "t0")))
    Condition
      Error:
      ! 'Made Up Class' is not an exported object from 'namespace:t0'
    Code
      new_class("Foo", properties = list(bar = new_class("Made Up Class", package = "Made Up Package")))
    Condition
      Error in `loadNamespace()`:
      ! there is no package called 'Made Up Package'
    Code
      modified_class <- t0::`An S7 Class`
      attr(modified_class, "xyz") <- "abc"
      new_class("Foo", properties = list(bar = modified_class))
    Condition
      Error:
      ! `t0::An S7 Class` is not identical to the class with the same @package and @name properties

