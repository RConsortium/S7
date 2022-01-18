# R7_class: can be printed

    Code
      my_class
    Output
      <R7_class>
      @name my_class
      @parent <R7_object>
      @properties

# classes can't inherit from S4 or class unions

    Code
      new_class("test", parent = parentS4)
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type
    Code
      new_class("test", parent = new_union("character"))
    Error <simpleError>
      `parent` must be an R7 class, S3 class, or base type

# constructor  types check their values

    `.data` must be <integer> not <character>

