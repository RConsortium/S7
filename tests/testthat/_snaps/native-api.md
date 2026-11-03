# native property access reports errors

    Code
      api$get_prop(x, "missing")
    Condition
      Error in `<S7::Foo>@missing`:
      ! Property not found.

---

    Code
      api$get_prop(x, "broken")
    Condition
      Error in `<Foo>@broken`:
      ! broken

---

    Code
      api$get_prop(1, "value")
    Condition
      Error in `api$get_prop()`:
      ! `1` must be an <S7_object>, not a <double>.
