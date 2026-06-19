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

