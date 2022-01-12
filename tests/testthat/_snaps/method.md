# method errors on invalid inputs

    `signature` must be a list of <R7_class> or a <character>:
    - `signature[1]`: is <numeric>

---

    `signature` must be a list of <R7_class> or a <character>:
    - `signature[1]`: is <numeric>

---

    `signature` must be a list of <R7_class> or a <character>:
    - `signature[1]`: is <logical>
    - `signature[2]`: is <logical>

# method errors if no method is defined for that class

    Can't find method for generic `foo()` with classes:
    - x: <blah>

# method_compatible throws errors if the functions are not compatible

    `method` must be consistent with <R7_generic> foo.
    - Argument 1 in generic is `x = `
    - Argument 1 in method is `y = `

---

    `method` must be consistent with <R7_generic> foo.
    - Argument 1 in generic is `x = `
    - Argument 1 in method is `x = "foo"`

---

    `method` must be consistent with <R7_generic> bar.
    - Argument 1 in generic is `x = `
    - Argument 1 in method is `y = `

---

    `method` must be consistent with <R7_generic> bar.
    - Argument 2 in generic is `y = `
    - Argument 2 in method is `y = NULL`

# R7_method printing

    Code
      method(foo, list(text, "integer"))
    Output
      <R7_method> method(foo, list(text, "integer"))
      function (x, y, ...) 
      paste0("bar:", x, y)
      <environment: 0x0>

