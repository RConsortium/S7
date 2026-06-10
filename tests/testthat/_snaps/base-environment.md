# can inherit from environments

    Code
      S7_data(e)
    Condition
      Error in `S7_data()`:
      ! Can't call `S7_data()` on an environment.
      See ?class_environment for details.
    Code
      S7_data(e) <- new.env()
    Condition
      Error in `S7_data<-`:
      ! Can't call `S7_data<-` on an environment.
      See ?class_environment for details.

# str() and print() work for environment-derived classes

    Code
      str(e)
    Output
      <Foo> <environment: 0x0>
       @ name: chr "bob"
    Code
      print(e)
    Output
      <Foo> <environment: 0x0>
       @ name: chr "bob"

# can't upcast an environment

    Code
      convert(Child(), Parent)
    Condition
      Error in `convert()`:
      ! Can't call `convert()` on an environment.
      See ?class_environment for details.

