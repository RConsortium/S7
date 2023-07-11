# can standardise generics

    Code
      as_generic(function() { })
    Error <simpleError>
      `generic` is a function, but not an S3 generic function

---

    Code
      as_generic(1)
    Error <simpleError>
      `generic` must be a function, not a <double>

# clear error if can't find generic

    Code
      find_S3_package(tibble::as_tibble, "as_tibble")
    Error <simpleError>
      Can't find package that generic `as_tibble` belongs to.
      Did you import the generic into the namespace?

