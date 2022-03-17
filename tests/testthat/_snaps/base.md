# validation uses typeof

    Code
      class_integer$validator(TRUE)
    Output
      [1] "Underlying data must be <integer> not <logical>"

# base class display as expected

    Code
      class_integer
    Output
      <R7_base_class>: <integer>
    Code
      str(class_integer)
    Output
      <R7_base_class>: <integer>

# environments: can be printed

    Code
      env(x = 1)
    Output
      <env> <environment: 0x0> 
       @ x: num 1

