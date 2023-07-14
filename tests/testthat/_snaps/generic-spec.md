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

