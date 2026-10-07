# deprecated_generic() warns then delegates to the replacement

    Code
      out <- old_gen(c(1, 2, 3))
    Condition
      Warning in `old_gen()`:
      `old_gen()` was deprecated in S7 1.1.0.
      Please use `new_gen()` instead.

# deprecated wrappers keep arguments separate from deprecation state

    Code
      out <- local({
        target <- 1
        old_gen(target = target, method = 2, when = 3, what = 4, with = 5, package = 6,
          env = 7, call = 8)
      })
    Condition
      Warning in `old_gen()`:
      `old_gen()` was deprecated in S7 1.0.0.
      Please use `new_gen()` instead.

---

    Code
      obj <- local({
        target <- 1
        Old(target = target)
      })
    Condition
      Warning in `Old()`:
      `Old()` was deprecated in S7 1.0.0.
      Please use `Target()` instead.

# deprecated wrappers preserve lazy arguments and target defaults

    Code
      out <- old_gen({
        calls <- calls + 1L
        2
      }, z = 3)
    Condition
      Warning in `old_gen()`:
      `old_gen()` was deprecated in S7 1.0.0.
      Please use `new_gen()` instead.

# deprecated_generic() without a replacement still dispatches

    Code
      out <- old_gen("hi")
    Condition
      Warning in `old_gen()`:
      `old_gen()` was deprecated in S7 2.0.0.

# deprecated_generic() accepts a custom generic definition

    Code
      out <- combine("a", "b")
    Condition
      Warning in `combine()`:
      `combine()` was deprecated in S7 2.0.0.

# external methods register on a directly defined deprecated generic

    Code
      out <- pkgA$old_gen("hi")
    Condition
      Warning in `pkgA$old_gen()`:
      `old_gen()` was deprecated in pkgA 2.0.0.

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
      ! argument "dispatch_args" is missing, with no default
    Code
      deprecated_generic("old_gen", dispatch_args = 1, when = "1.0.0")
    Condition
      Error in `S7::new_generic()`:
      ! `dispatch_args` must be a character vector.
    Code
      deprecated_generic("old_gen", "x", function(x) x, when = "1.0.0")
    Condition
      Error in `S7::new_generic()`:
      ! `fun` must contain a call to `S7_dispatch()`.
    Code
      deprecated_generic("old_gen", "x", new = new_gen, when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! Can't supply `dispatch_args` or `fun` with `new`.
    Code
      deprecated_generic("old_gen", fun = function(x) S7_dispatch(), new = new_gen,
      when = "1.0.0")
    Condition
      Error in `deprecated_generic()`:
      ! Can't supply `dispatch_args` or `fun` with `new`.
    Code
      deprecated_generic("old_gen", new = new_gen, when = "1.0.0", new_label = 1)
    Condition
      Error in `deprecated_generic()`:
      ! `new_label` must be a single string.
    Code
      deprecated_generic("old_gen", new = new_gen, when = "1.0.0", new_label = "")
    Condition
      Error in `deprecated_generic()`:
      ! `new_label` must not be "" or NA.
    Code
      deprecated_generic("old_gen", "x", when = "1.0.0", new_label = "x()")
    Condition
      Error in `deprecated_generic()`:
      ! `new_label` requires `new`.
    Code
      deprecated_generic("old_gen", new = new_gen, when = "1.0.0", method = "warn")
    Condition
      Error in `deprecated_generic()`:
      ! `method` must be one of "base", "lifecycle(warn)", or "lifecycle(stop)".

# deprecated_class() warns without changing the class

    Code
      d <- Dog(name = "Fido")
    Condition
      Warning in `Dog()`:
      `Dog()` was deprecated in S7 2.0.0.
      Please use `Pet()` instead.

---

    Code
      expect_no_warning(print(d))
    Output
      <S7::Dog>
       @ name: chr "Fido"

# deprecated classes keep their own methods and subclasses

    Code
      speak(Pet(name = "Rex"))
    Condition
      Error:
      ! Can't find method for `speak(<S7::Pet>)`.

# deprecated_class() without a replacement still constructs

    Code
      felix <- Cat(lives = 9)
    Condition
      Warning in `Cat()`:
      `Cat()` was deprecated in S7 3.0.0.

# replacement labels can preserve generic identities

    Code
      out <- foo(x)
    Condition
      Warning in `foo()`:
      `foo()` was deprecated in S7 2.0.0.
      Please use `bar()` instead.

---

    Code
      print(foo)
    Output
      <S7_deprecated_generic> `foo()` was deprecated in S7 2.0.0. Please use `bar()` instead.

---

    Code
      print(older)
    Output
      <S7_deprecated_generic> `older()` was deprecated in S7 3.0.0. Please use `bar()` instead.

# replacement labels work with lifecycle

    Code
      foo(1)
    Condition
      Error:
      ! `foo()` was deprecated in S7 2.0.0 and is now defunct.
      i Please use `bar()` instead.

# deprecated classes preserve constructor scope and validation

    Code
      Dog()
    Condition
      Error:
      ! `Dog()` was deprecated in S7 2.0.0 and is now defunct.
      i Please use `Pet()` instead.

---

    Code
      Puppy(name = character())
    Condition
      Error in `Dog()`:
      ! <S7::Dog> object is invalid:
      - name must have length 1

# installed direct property defaults need rebuilding after deprecation

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      BaseBox
      `Base()` was deprecated in deprecatedDefaults 2.0.0.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      BaseExplicit
      `Base()` was deprecated in deprecatedDefaults 2.0.0.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      WarnBox
      `Warn()` was deprecated in deprecatedDefaults 2.0.0.
      i The deprecated feature was likely used in the deprecatedDefaultsUser package.
        Please report the issue to the authors.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      WarnExplicit
      `Warn()` was deprecated in deprecatedDefaults 2.0.0.
      i The deprecated feature was likely used in the deprecatedDefaultsUser package.
        Please report the issue to the authors.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      StopBox
      `Stop()` was deprecated in deprecatedDefaults 2.0.0 and is now defunct.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      StopExplicit
      `Stop()` was deprecated in deprecatedDefaults 2.0.0 and is now defunct.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      BaseExplicit
      `Base()` was deprecated in deprecatedDefaults 2.0.0.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      WarnExplicit
      `Warn()` was deprecated in deprecatedDefaults 2.0.0.
      i The deprecated feature was likely used in the deprecatedDefaultsUser package.
        Please report the issue to the authors.

---

    Code
      cat(c(name, out$warnings, out$error), sep = "\n")
    Output
      StopExplicit
      `Stop()` was deprecated in deprecatedDefaults 2.0.0 and is now defunct.

# deprecated classes name replacements from other packages

    Code
      invisible(Dog())
    Condition
      Warning in `Dog()`:
      `Dog()` was deprecated in S7 2.0.0.
      Please use `dep::Pet()` instead.

# deprecated classes preserve S4 parents

    Code
      obj <- Old(value = 1)
    Condition
      Warning in `Old()`:
      `Old()` was deprecated in version 1.0.0.

# deprecated_class() validates its inputs

    Code
      deprecated_class(name = 1, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `name` must be a single string.
    Code
      deprecated_class(name = "Old")
    Condition
      Error in `deprecated_class()`:
      ! argument "when" is missing, with no default
    Code
      deprecated_class(name = "Old", when = "next year")
    Condition
      Error in `deprecated_class()`:
      ! `when` must be a version number, not "next year".
    Code
      deprecated_class(name = "Old", when = "1.0.0", method = "warn")
    Condition
      Error in `deprecated_class()`:
      ! `method` must be one of "base", "lifecycle(warn)", or "lifecycle(stop)".
    Code
      deprecated_class(name = "Old", new = 1, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `new` must be an S7 class, not <double>.
    Code
      deprecated_class(name = "Old", new = class_double, when = "1.0.0")
    Condition
      Error in `deprecated_class()`:
      ! `new` must be an S7 class, not S3<S7_base_class>.

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

# deprecated_property() without a replacement validates stored values

    Code
      Hat(brim = "invalid")
    Condition
      Error in `<S7::Hat>@brim`:
      ! <S7::Hat>@brim must be <double>, not <character>

---

    Code
      h@brim <- "invalid"
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 1.0.0.
      Error in `<S7::Hat>@brim`:
      ! <S7::Hat>@brim must be <double>, not <character>

# deprecated_property() without a replacement preserves NULL

    Code
      value <- h@brim
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 1.0.0.

---

    Code
      h@brim <- 2
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 1.0.0.

# retired properties preserve validators

    Code
      Hat(brim = -1)
    Condition
      Error in `<S7::Hat>@brim`:
      ! <S7::Hat>@brim must be non-negative

---

    Code
      h@brim <- -1
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 1.0.0.
      Error in `<S7::Hat>@brim`:
      ! <S7::Hat>@brim must be non-negative

---

    Code
      h@brim <- 2
    Condition
      Warning:
      `<S7::Hat>@brim` was deprecated in S7 1.0.0.

# renamed properties reject validators

    Code
      deprecated_property("count", new = "size", when = "1.0.0", validator = function(
        value) if (value < 0) "must be non-negative")
    Condition
      Error in `deprecated_property()`:
      ! When `new` is supplied, put `validator` on the replacement property.

# equal property values are silent even with stopping deprecations

    Code
      b@count <- 4
    Condition
      Error:
      ! <S7::Basket>@count was deprecated in S7 1.0.0 and is now defunct.
      i Please use <S7::Basket>@size instead.

---

    Code
      b@retired <- 4
    Condition
      Error:
      ! <S7::Basket>@retired was deprecated in S7 1.0.0 and is now defunct.

---

    Code
      Basket(size = 2, count = 4)
    Condition
      Error:
      ! <S7::Basket>@count was deprecated in S7 1.0.0 and is now defunct.
      i Please use <S7::Basket>@size instead.

# deprecated properties are omitted from object printing

    Code
      expect_no_warning(print(b))
    Output
      <S7::Basket>
       @ size   : num 3
       @ doubled: num 6
    Code
      expect_no_warning(str(b))
    Output
      <S7::Basket>
       @ size   : num 3
       @ doubled: num 6
    Code
      expect_no_warning(str(list(b)))
    Output
      List of 1
       $ : <S7::Basket>
        ..@ size   : num 3
        ..@ doubled: num 6

---

    Code
      b@count
    Condition
      Warning:
      `<S7::Basket>@count` was deprecated in S7 1.5.0.
      Please use `<S7::Basket>@size` instead.
    Output
      [1] 3
    Code
      b@retired
    Condition
      Warning:
      `<S7::Basket>@retired` was deprecated in S7 1.5.0.
    Output
      [1] 1

# printing respects inherited and overridden deprecated properties

    Code
      expect_no_warning(print(Child()))
    Output
      <S7::Child>
    Code
      expect_no_warning(print(Visible(x = 1)))
    Output
      <S7::Visible>
       @ x: num 1

# printing skips properties that signal deprecation errors

    Code
      print(b)
    Output
      <S7::Basket>
       @ size: num 3
    Code
      str(b)
    Output
      <S7::Basket>
       @ size: num 3

---

    Code
      b@count
    Condition
      Error:
      ! <S7::Basket>@count was deprecated in S7 1.5.0 and is now defunct.
      i Please use <S7::Basket>@size instead.

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

# props() attributes repeated lifecycle warnings to its direct caller

    Code
      withCallingHandlers({
        invisible(props(x))
        invisible(props(x))
      }, lifecycle_warning_deprecated = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
      })
    Condition
      Warning:
      <directProps::Renamed>@count was deprecated in directProps 2.0.0.
      i Please use <directProps::Renamed>@size instead.
      Warning:
      <directProps::Renamed>@count was deprecated in directProps 2.0.0.
      i Please use <directProps::Renamed>@size instead.

---

    Code
      withCallingHandlers({
        invisible(props(x))
        invisible(props(x))
      }, lifecycle_warning_deprecated = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
      })
    Condition
      Warning:
      <directProps::Retired>@item was deprecated in directProps 2.0.0.
      Warning:
      <directProps::Retired>@item was deprecated in directProps 2.0.0.

# generated constructors attribute property deprecation to their caller

    Code
      withCallingHandlers({
        invisible(constructorProps$Renamed(size = 1, count = 2))
        invisible(constructorProps$Renamed(size = 1, count = 3))
      }, lifecycle_warning_deprecated = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
      })
    Condition
      Warning:
      <constructorProps::Renamed>@count was deprecated in constructorProps 2.0.0.
      i Please use <constructorProps::Renamed>@size instead.
      Warning:
      <constructorProps::Renamed>@count was deprecated in constructorProps 2.0.0.
      i Please use <constructorProps::Renamed>@size instead.

# props() attributes indirect lifecycle warnings to the downstream package

    Code
      cat(c(verbosity, result$first$warnings), sep = "\n")
    Output
      default
      <indirectProps::Renamed>@count was deprecated in indirectProps 2.0.0.
      i Please use <indirectProps::Renamed>@size instead.
      i The deprecated feature was likely used in the propsUser package.
        Please report the issue to the authors.

---

    Code
      cat(c(verbosity, result$first$warnings), sep = "\n")
    Output
      default
      <indirectProps::Retired>@item was deprecated in indirectProps 2.0.0.
      i The deprecated feature was likely used in the propsUser package.
        Please report the issue to the authors.

---

    Code
      cat(c(verbosity, result$first$warnings), sep = "\n")
    Output
      warning
      <indirectProps::Renamed>@count was deprecated in indirectProps 2.0.0.
      i Please use <indirectProps::Renamed>@size instead.
      i The deprecated feature was likely used in the propsUser package.
        Please report the issue to the authors.

---

    Code
      cat(c(verbosity, result$first$warnings), sep = "\n")
    Output
      warning
      <indirectProps::Retired>@item was deprecated in indirectProps 2.0.0.
      i The deprecated feature was likely used in the propsUser package.
        Please report the issue to the authors.

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
