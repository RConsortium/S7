# S7_on_load() doesn't duplicate hooks when registrars error

    Code
      downstream$.onLoad()
    Condition
      Error:
      ! Can't find external class <hookclasspkg::Missing>:
      * Packages 'hookclasspkg' doesn't contain 'Missing'.

---

    Code
      downstream$.onLoad()
    Condition
      Error:
      ! Can't find external class <hookclasspkg::Missing>:
      * Packages 'hookclasspkg' doesn't contain 'Missing'.

# S7_on_load() removes stale hooks when hook records are lost

    Code
      downstream$.onLoad()
    Condition
      Error:
      ! Can't find external class <hooklostclasspkg::Missing>:
      * Packages 'hooklostclasspkg' doesn't contain 'Missing'.

---

    Code
      downstream$.onLoad()
    Condition
      Error:
      ! Can't find external class <hooklostclasspkg::Missing>:
      * Packages 'hooklostclasspkg' doesn't contain 'Missing'.

# S7_on_load() does not partially register unions when an arm errors

    Code
      downstream$.onLoad()
    Condition
      Error:
      ! Can't find external class <upstream_external_union_error_b::B>:
      * Packages 'upstream_external_union_error_b' doesn't contain 'B'.

---

    Code
      generic_pkg$gen(upstream_a$A())
    Condition
      Error:
      ! Can't find method for `gen(<upstream_external_union_error_a::A>)`.

