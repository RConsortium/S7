# errors from dispatched methods have reasonable tracebacks

    Code
      my_generic(10)
    Output
      [[1]]
      my_generic(10)
      
      [[2]]
      S7::S7_dispatch()
      
      [[3]]
      `method(my_generic, class_double)`(x = 10)
      

---

    Code
      my_generic(3, 4)
    Output
      [[1]]
      my_generic(3, 4)
      
      [[2]]
      S7::S7_dispatch()
      
      [[3]]
      `method(my_generic, list(class_double, class_double))`(x = 3, 
          y = 4)
      

# method error calls preserve expressions on R < 4.4

    Code
      g(Foo(), stop("unused"), extra = stop("extra"), z = identity(2))
    Condition
      Error in `method(g, Foo)`:
      ! boom

# method calls and warnings preserve expressions on R < 4.4

    Code
      g(Foo())
    Condition
      Warning in `method(g, Foo)`:
      boom

