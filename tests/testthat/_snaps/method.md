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
    - x: <logical>
    - y: <list>

---

    Can't find method for generic `foo()` with classes:
    - x: <logical>
    - y: <>

# method lookup fails with an informative message for multiple classes

    Can't find method for generic `foo()` with classes:
    - x: <tbl_df>, <tbl>, <data.frame>
    - y: <POSIXct>, <POSIXt>

