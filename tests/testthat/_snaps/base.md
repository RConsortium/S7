# validation uses typeof

    Code
      class_integer$validator(TRUE)
    Output
      [1] "Underlying data must be <integer> not <logical>"

# base class display as expected

    Code
      base_classes
    Output
      [[1]]
      <R7_base_class>: <logical>
      
      [[2]]
      <R7_base_class>: <integer>
      
      [[3]]
      <R7_base_class>: <double>
      
      [[4]]
      <R7_base_class>: <complex>
      
      [[5]]
      <R7_base_class>: <character>
      
      [[6]]
      <R7_base_class>: <raw>
      
      [[7]]
      <R7_base_class>: <expression>
      
      [[8]]
      <R7_base_class>: <list>
      
      [[9]]
      <R7_base_class>: <function>
      
      [[10]]
      <R7_base_class>: <environment>
      
      [[11]]
      <R7_base_class>: <expression>
      
    Code
      str(base_classes)
    Output
      List of 11
       $ : <R7_base_class>: <logical>
       $ : <R7_base_class>: <integer>
       $ : <R7_base_class>: <double>
       $ : <R7_base_class>: <complex>
       $ : <R7_base_class>: <character>
       $ : <R7_base_class>: <raw>
       $ : <R7_base_class>: <expression>
       $ : <R7_base_class>: <list>
       $ : <R7_base_class>: <function>
       $ : <R7_base_class>: <environment>
       $ : <R7_base_class>: <expression>

# environments: can be printed

    Code
      env(x = 1)
    Output
      <env> <environment: 0x0> 
       @ x: num 1

