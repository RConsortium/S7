# validate() validates object and type recursively

    Code
      obj <- klass(1, -1)
      attr(obj, "x") <- -1
      validate(obj)
    Condition
      Error in `validate()`:
      ! <klass> object is invalid:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      validate(obj)
    Condition
      Error in `validate()`:
      ! <klass> object properties are invalid:
      - @x must be <double>, not <character>

---

    Code
      obj <- klass2(1, -1, 1)
      attr(obj, "x") <- -1
      validate(obj)
    Condition
      Error in `validate()`:
      ! <klass2> object is invalid:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      attr(obj, "z") <- "y"
      validate(obj)
    Condition
      Error in `validate()`:
      ! <klass2> object properties are invalid:
      - @x must be <double>, not <character>
      - @z must be <double>, not <character>

# validate checks base type

    Code
      validate(x)
    Condition
      Error in `validate()`:
      ! <Double> object is invalid:
      - Underlying data must be <double> not <character>

# validate runs property validators for base type properties

    Code
      Positive(x = -1)
    Condition
      Error in `Positive()`:
      ! <Positive> object properties are invalid:
      - @x must be positive

# validate runs class validators for non-base type properties

    Code
      validate(obj)
    Condition
      Error in `validate()`:
      ! <Wrapper> object properties are invalid:
      - @x: attr(, 'levels') must be a <character>
      - @x: Not enough 'levels' for underlying data

# validate checks the type of setters

    Code
      foo(x = 123)
    Condition
      Error in `<foo>@x`:
      ! <foo>@x must be <double>, not <character>

