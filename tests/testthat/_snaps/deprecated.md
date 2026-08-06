# deprecated_generic() warns then delegates to the replacement

    Code
      out <- old_gen(c(1, 2, 3))
    Condition
      Warning in `old_gen()`:
      `old_gen()` was deprecated in S7 1.1.0.
      Please use `new_gen()` instead.

# deprecated_generic() without a replacement still dispatches

    Code
      out <- old_gen("hi")
    Condition
      Warning in `old_gen()`:
      `old_gen()` was deprecated in S7 2.0.0.

# deprecated_generic() validates its inputs

    Code
      deprecated_generic(1, new = new_gen, when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! `name` must be a single string.
    Code
      deprecated_generic("old_gen", new = new_gen)
    Condition
      Error in `deprecated_generic()`:
      ! argument "when" is missing, with no default
    Code
      deprecated_generic("old_gen", new = new_gen, when = "next year")
    Condition
      Error in `deprecated_generic()`:
      ! `when` must be a version number, not "next year".
    Code
      deprecated_generic("old_gen", new = mean, when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! `new` must be an S7 generic, not <closure>.
    Code
      deprecated_generic("old_gen", when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! Must supply exactly one of `new` and `old`.
    Code
      deprecated_generic("old_gen", new = new_gen, old = new_gen, when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! Must supply exactly one of `new` and `old`.
    Code
      deprecated_generic("old_gen", old = mean, when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! `old` must be an S7 generic, not <closure>.
    Code
      deprecated_generic("old_gen", old = new_gen, when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! `old@name` ("new_gen") must match `name` ("old_gen").
      * To deprecate in favor of a renamed generic, use `new`.
    Code
      deprecated_generic("old_gen", new = new_gen, when = "1.0.0", method = "warn")
    Condition
      Error in `deprecated_generic()`:
      ! `method` must be one of "base", "lifecycle(warn)", or "lifecycle(stop)".

# deprecated_class() constructor warns then constructs the replacement

    Code
      d <- Dog(name = "Fido")
    Condition
      Warning in `Dog()`:
      `Dog()` was deprecated in S7 2.0.0.
      Please use `Pet()` instead.

# deprecated_class() without a replacement still constructs

    Code
      felix <- Cat(lives = 9)
    Condition
      Warning in `Cat()`:
      `Cat()` was deprecated in S7 3.0.0.

# deprecated_class() validates its inputs

    Code
      deprecated_class(1, new = Pet, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `name` must be a single string.
    Code
      deprecated_class("Old", new = Pet)
    Condition
      Error in `deprecated_class()`:
      ! argument "when" is missing, with no default
    Code
      deprecated_class("Old", new = 1, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `new` must be an S7 class, not <double>.
    Code
      deprecated_class("Old", when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! Must supply exactly one of `new` and `old`.
    Code
      deprecated_class("Old", new = Pet, old = Pet, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! Must supply exactly one of `new` and `old`.
    Code
      deprecated_class("Old", old = 1, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `old` must be an S7 class, not <double>.
    Code
      deprecated_class("Old", old = Pet, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `old@name` ("Pet") must match `name` ("Old").
      * To deprecate in favor of a renamed class, use `new`.

# deprecated_property() with a replacement delegates and warns

    Code
      print(b@count)
    Condition
      Warning:
      `<S7::Basket>@count` was deprecated in S7 1.5.0.
      Please use `<S7::Basket>@size` instead.
    Output
      [1] 3
    Code
      b@count <- 5
    Condition
      Warning:
      `<S7::Basket>@count` was deprecated in S7 1.5.0.
      Please use `<S7::Basket>@size` instead.

# deprecated_property() only warns at construction when actually used

    Code
      b <- Basket(count = 7)
    Condition
      Warning:
      `<S7::Basket>@count` was deprecated in S7 1.5.0.
      Please use `<S7::Basket>@size` instead.

# deprecated_property() without a replacement still stores data

    Code
      print(h@brim)
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 0.9.0.
    Output
      [1] 2
    Code
      h@brim <- 3
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 0.9.0.

# deprecated_property() validates its inputs

    Code
      deprecated_property(1, when = "1.0.0")
    Condition
      Error in `deprecated_property()`:
      ! `old` must be a single string.
    Code
      deprecated_property("count", new = 1, when = "1.0.0")
    Condition
      Error in `deprecated_property()`:
      ! `new` must be a single string.
    Code
      deprecated_property("count", new = "size")
    Condition
      Error in `deprecated_property()`:
      ! argument "when" is missing, with no default
    Code
      deprecated_property("count", new = "size", when = "next year")
    Condition
      Error in `deprecated_property()`:
      ! `when` must be a version number, not "next year".
    Code
      deprecated_property("count", new = "size", when = "1.0.0", method = "warn")
    Condition
      Error in `deprecated_property()`:
      ! `method` must be one of "base", "lifecycle(warn)", or "lifecycle(stop)".

# deprecation warnings mention the package

    Code
      invisible(pkg$old_gen(1))
    Condition
      Warning in `pkg$old_gen()`:
      `old_gen()` was deprecated in pkgA 1.1.0.
      Please use `new_gen()` instead.

# method = 'lifecycle(warn)' signals with lifecycle

    Code
      invisible(pkg$old_gen(1))
    Condition
      Warning:
      `old_gen()` was deprecated in pkgA 1.1.0.
      i Please use `new_gen()` instead.

# method = 'lifecycle(stop)' errors

    Code
      old_gen(1)
    Condition
      Error:
      ! `old_gen()` was deprecated in S7 1.1.0 and is now defunct.
      i Please use `new_gen()` instead.

# deprecated_property() works with lifecycle

    Code
      invisible(b@count)
    Condition
      Warning:
      <S7::Basket>@count was deprecated in S7 1.5.0.
      i Please use <S7::Basket>@size instead.

# deprecated generics and classes print nicely

    Code
      print(old_gen)
    Output
      <S7_deprecated_generic> `old_gen()` was deprecated in S7 1.1.0. Please use `new_gen()` instead.
    Code
      print(Dog)
    Output
      <S7_deprecated_class> `Dog()` was deprecated in S7 2.0.0. Please use `Pet()` instead.
    Code
      print(Cat)
    Output
      <S7_deprecated_class> `Cat()` was deprecated in S7 3.0.0.
