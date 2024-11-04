# displays nicely

    Code
      print(bar)
    Output
      <S7_external_generic> foo::bar(x)

# new_method works with both hard and soft dependencies

    Code
      args(Foo)
    Output
      function (bar = t0::AnS7Class()) 
      NULL
    Code
      args(t2::AnS7Class2)
    Output
      function (bar = t0::AnS7Class()) 
      NULL
    Code
      args(t2:::AnInternalClass)
    Output
      function (foo = t0::AnS7Class(), bar = AnS7Class2()) 
      NULL

---

    Code
      new_class("Foo", properties = list(bar = new_class("MadeUpClass", package = "t0")))
    Condition
      Error:
      ! 'MadeUpClass' is not an exported object from 'namespace:t0'
    Code
      new_class("Foo", properties = list(bar = new_class("MadeUpClass", package = "MadeUpPackage")))
    Condition
      Error in `loadNamespace()`:
      ! there is no package called 'MadeUpPackage'
    Code
      modified_class <- t0::AnS7Class
      attr(modified_class, "xyz") <- "abc"
      new_class("Foo", properties = list(bar = modified_class))
    Condition
      Error:
      ! `t0::AnS7Class` is not identical to the class with the same @package and @name properties

