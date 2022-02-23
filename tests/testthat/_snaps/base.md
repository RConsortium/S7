# validation uses typeof

    Code
      base_classes$integer$validator(TRUE)
    Output
      [1] "Underlying data must be <integer> not <logical>"

# base class display as expected

    Code
      base_classes
    Output
      $logical
      <R7_base_class>: <logical>
      
      $integer
      <R7_base_class>: <integer>
      
      $double
      <R7_base_class>: <double>
      
      $complex
      <R7_base_class>: <complex>
      
      $character
      <R7_base_class>: <character>
      
      $raw
      <R7_base_class>: <raw>
      
      $list
      <R7_base_class>: <list>
      
      $expression
      <R7_base_class>: <expression>
      
      $`function`
      <R7_base_class>: <function>
      
      $environment
      <R7_base_class>: <environment>
      
    Code
      str(base_classes)
    Output
      List of 10
       $ logical    : <R7_base_class>: <logical>
       $ integer    : <R7_base_class>: <integer>
       $ double     : <R7_base_class>: <double>
       $ complex    : <R7_base_class>: <complex>
       $ character  : <R7_base_class>: <character>
       $ raw        : <R7_base_class>: <raw>
       $ list       : <R7_base_class>: <list>
       $ expression : <R7_base_class>: <expression>
       $ function   : <R7_base_class>: <function>
       $ environment: <R7_base_class>: <environment>

# environments: can be printed

    Code
      env(x = 1)
    Output
      <env><environment: 0x0> 
      @ x:  num 1

