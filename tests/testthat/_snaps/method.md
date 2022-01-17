# method errors on invalid inputs

    signature[[1]]: class specification must by a R7 class object, result of s3_class(), a S4 class object, or a base constructor function, not a <double>.

---

    signature[[1]]: class specification must by a R7 class object, result of s3_class(), a S4 class object, or a base constructor function, not a <double>.

---

    signature[[1]]: class specification must by a R7 class object, result of s3_class(), a S4 class object, or a base constructor function, not a <logical>.

# method errors if no method is defined for that class

    signature[[1]]: Can't find base class called 'blah'

# method_compatible throws errors if the functions are not compatible

    `method` must be consistent with <R7_generic> foo.
    - Argument 1 in generic is `x = `
    - Argument 1 in method is `y = `

---

    `method` must be consistent with <R7_generic> foo.
    - `generic` has `...`
    - `method` does not have `...`

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
    - `generic` has `...`
    - `method` does not have `...`

---

    `method` must be consistent with <R7_generic> bar.
    - Argument 2 in generic is `y = `
    - Argument 2 in method is `y = NULL`

# method compatible verifies that if a generic does not have dots the method should not have dots

    `method` must be consistent with <R7_generic> foo.
    - `generic` does not have `...`
    - `method` has `...`

# method lookup fails with an informative message for single classes

    Can't find method for generic `foo()` with classes:
    - x: <character>
    - y: <character>

---

    Can't find method for generic `foo()` with classes:
    - x: <character>
    - y: <NULL>

# method lookup fails with an informative message for multiple classes

    Can't find method for generic `foo()` with classes:
    - x: <character>
    - y: <character>

# R7_method printing

    Code
      method(foo, list(text, "integer"))
    Output
      <R7_method> method(foo, list(text, "integer"))
      function (x, y, ...) 
      paste0("bar:", x, y)
      <environment: 0x0>

