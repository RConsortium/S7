# validate() validates object and type recursively

    Code
      obj <- klass(1, -1)
      attr(obj, "x") <- -1
      validate(obj)
    Error <simpleError>
      <klass> object is invalid:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      validate(obj)
    Error <simpleError>
      <klass> object properties are invalid:
      - @x must be <double>, not <character>

---

    Code
      obj <- klass2(1, -1, 1)
      attr(obj, "x") <- -1
      validate(obj)
    Error <simpleError>
      <klass2> object is invalid:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      attr(obj, "z") <- "y"
      validate(obj)
    Error <simpleError>
      <klass2> object properties are invalid:
      - @x must be <double>, not <character>
      - @z must be <double>, not <character>

# validate checks base type

    Code
      validate(x)
    Error <simpleError>
      <Double> object is invalid:
      - Underlying data must be <double> not <character>

# validate checks the type of setters

    Code
      foo(x = 123)
    Error <simpleError>
      <foo>@x must be <double>, not <character>

