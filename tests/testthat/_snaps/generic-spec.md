# can standardise generics

    Code
      as_generic(function() { })
    Condition
      Error:
      ! `generic` is a function, but not an S3 generic function: 
      function () 
      {
      }

---

    Code
      as_generic(1)
    Condition
      Error:
      ! `generic` must be a function, not a <double>

