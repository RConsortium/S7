# validate() validates object and type recursively

    Code
      obj <- klass(1, -1)
      attr(obj, "x") <- -1
      validate(obj)
    Error <simpleError>
      Invalid <klass> object:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      validate(obj)
    Error <simpleError>
      Invalid <klass> object:
      - <klass>@x must be of class <double>, not <character>

---

    Code
      obj <- klass2(1, -1, 1)
      attr(obj, "x") <- -1
      validate(obj)
    Error <simpleError>
      Invalid <klass2> object:
      - x must be positive
    Code
      attr(obj, "x") <- "y"
      attr(obj, "z") <- "y"
      validate(obj)
    Error <simpleError>
      Invalid <klass2> object:
      - <klass2>@x must be of class <double>, not <character>
      - <klass2>@z must be of class <double>, not <character>

