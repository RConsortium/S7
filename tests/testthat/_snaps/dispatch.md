# generics pass ... to methods

    unused argument (baz = "/")

# method lookup fails with informative messages

    Can't find method for generic `foo()` with classes:
    - x: <logical>
    - y: <MISSING>

---

    Can't find method for generic `foo()` with classes:
    - x: <logical>
    - y: <list>

---

    Can't find method for generic `foo()` with classes:
    - x: <tbl_df>, <tbl>, <data.frame>
    - y: <POSIXct>, <POSIXt>

