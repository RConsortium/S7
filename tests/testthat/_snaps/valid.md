# validate() validates object and type recursively

    Code
      obj <- klass(1, -1)
      attr(obj, "x") <- -1
      validate(obj)
    Error <simpleError>
      R7<klass> object is invalid:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      validate(obj)
    Error <simpleError>
      R7<klass> object properties are invalid:
      - R7<klass>@x must be <double>, not <character>

---

    Code
      obj <- klass2(1, -1, 1)
      attr(obj, "x") <- -1
      validate(obj)
    Error <simpleError>
      R7<klass2> object is invalid:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      attr(obj, "z") <- "y"
      validate(obj)
    Error <simpleError>
      R7<klass2> object properties are invalid:
      - R7<klass2>@x must be <double>, not <character>
      - R7<klass2>@z must be <double>, not <character>

# validate checks base type

    Code
      validate(x)
    Error <simpleError>
      R7<Double> object is invalid:
      - Underlying data must be <double> not <character>

