# collect_dots() errors if arguments are unnamed

    Code
      collect_dots(1, 2)
    Condition
      Error:
      ! All arguments to `...` must be named.
    Code
      collect_dots(list(1, y = 2))
    Condition
      Error:
      ! All elements of `..1` must be named.

# check_function() accepts a matching single signature

    Code
      check_function(1, alist(x = ), arg = "f")
    Condition
      Error:
      ! `f` must be a function.

---

    Code
      check_function(function(y) { }, alist(x = ), arg = "f")
    Condition
      Error:
      ! `f` must be function(x), not function(y).

# check_function() accepts any of several candidate signatures

    Code
      check_function(function(x, y, z) { }, sigs, arg = "setter")
    Condition
      Error:
      ! `setter` must be function(self, value) or function(self, name, value), not function(x, y, z).

