test_that("deprecated_generic() warns then delegates to the replacement", {
  new_gen := new_generic("x")
  method(new_gen, class_double) <- function(x) mean(x)
  old_gen := deprecated_generic(new = new_gen, when = "1.1.0")

  expect_snapshot(out <- old_gen(c(1, 2, 3)))
  expect_equal(out, 2)
  expect_identical(formals(old_gen), formals(new_gen))
})

test_that("deprecated wrappers keep arguments separate from deprecation state", {
  new_gen := new_generic(
    "target",
    \(target = -1, method, when, what, with, package, env, call) S7_dispatch()
  )
  method(new_gen, class_double) <- function(
    target = -1,
    method,
    when,
    what,
    with,
    package,
    env,
    call
  ) {
    list(target, method, when, what, with, package, env, call)
  }
  old_gen := deprecated_generic(new = new_gen, when = "1.0.0")

  target <- -2
  expect_snapshot(
    out <- local({
      target <- 1
      old_gen(
        target = target,
        method = 2,
        when = 3,
        what = 4,
        with = 5,
        package = 6,
        env = 7,
        call = 8
      )
    })
  )
  expect_identical(out, list(1, 2, 3, 4, 5, 6, 7, 8))

  Target := new_class(
    properties = list(target = new_property(class_double, default = -1))
  )
  Old := deprecated_class(
    properties = list(target = new_property(class_double, default = -1)),
    new = Target,
    when = "1.0.0"
  )
  expect_snapshot(
    obj <- local({
      target <- 1
      Old(target = target)
    })
  )
  expect_identical(obj@target, 1)
  expect_identical(S7_class(obj)@name, "Old")
})

test_that("deprecated wrappers preserve lazy arguments and target defaults", {
  new_gen := new_generic(
    "x",
    \(x, unused = stop("unused"), y = x, ...) S7_dispatch()
  )
  method(new_gen, class_double) <- function(
    x,
    unused = stop("unused"),
    y = x,
    ...
  ) {
    list(x = x, y = y, dots = list(...))
  }
  old_gen := deprecated_generic(new = new_gen, when = "1.0.0")
  calls <- 0L
  expect_snapshot(
    out <- old_gen(
      {
        calls <- calls + 1L
        2
      },
      z = 3
    )
  )
  expect_identical(out, list(x = 2, y = 2, dots = list(z = 3)))
  expect_identical(calls, 1L)
})

test_that("method registration on a deprecated generic targets the replacement", {
  new_gen := new_generic("x")
  old_gen := deprecated_generic(new = new_gen, when = "1.1.0")

  method(old_gen, class_character) <- function(x) toupper(x)
  expect_equal(new_gen("hi"), "HI")

  # method() and S7_methods() introspection unwrap too
  expect_equal(method(old_gen, class_character)(x = "hi"), "HI")
  expect_equal(S7_methods(old_gen)$generic, "new_gen")
})

test_that("deprecated_generic() without a replacement still dispatches", {
  old_gen := deprecated_generic("x", when = "2.0.0")
  expect_no_warning(method(old_gen, class_character) <- \(x) toupper(x))

  expect_snapshot(out <- old_gen("hi"))
  expect_equal(out, "HI")
  expect_equal(method(old_gen, class_character)(x = "hi"), "HI")
  expect_equal(S7_methods(old_gen)$generic, "old_gen")
})

test_that("deprecated_generic() accepts a custom generic definition", {
  suffix <- "!"
  calls <- 0L
  fun <- function(x, y, ..., ending = suffix) {
    calls <<- calls + 1L
    S7_dispatch()
  }
  combine := deprecated_generic(c("x", "y"), fun, when = "2.0.0")
  method(combine, list(class_character, class_character)) <- function(
    x,
    y,
    ...,
    ending = suffix
  ) {
    paste0(x, y, ending)
  }

  expect_identical(formals(combine), formals(fun))
  expect_snapshot(out <- combine("a", "b"))
  expect_equal(out, "ab!")
  expect_identical(calls, 1L)
})

test_that("external methods register on a directly defined deprecated generic", {
  pkgA := local_package({
    old_gen := deprecated_generic("x", when = "2.0.0")
  })
  pkgB := local_package({
    old_gen := new_external_generic("pkgA", dispatch_args = "x")
    method(old_gen, class_character) <- \(x) toupper(x)
  })

  expect_snapshot(out <- pkgA$old_gen("hi"))
  expect_equal(out, "HI")
  expect_identical(
    environment(method(pkgA$old_gen, class_character)@generic),
    pkgA
  )
})

test_that("external generic registration resolves through a deprecated generic", {
  local_package("pkgA", {
    new_gen := new_generic("x")
    old_gen := deprecated_generic(new = new_gen, when = "1.1.0")
  })
  local_package("pkgB", {
    old_gen := new_external_generic("pkgA", dispatch_args = "x")
    method(old_gen, class_character) <- function(x) toupper(x)
  })

  expect_equal(asNamespace("pkgA")$new_gen("hi"), "HI")
})

test_that("installed deprecations preserve generic methods and existing classes", {
  # Relies on installed S7, including in the package installation subprocesses.
  skip_if(quick_test())
  lib <- local_libpath()
  fixtures <- test_path("deprecated")
  quick_install(file.path(fixtures, c("home-v1", "user")), lib)
  expect_identical(
    callr::r(
      function() {
        loadNamespace("deprecatedUser")
        c(
          deprecatedHome::gen(1),
          deprecatedHome::gen("x"),
          deprecatedHome::shout("hi")
        )
      },
      libpath = .libPaths()
    ),
    c("double", "character", "HI")
  )
  quick_install(file.path(fixtures, c("core", "home-v2")), lib)

  check <- function() {
    loadNamespace("deprecatedUser")
    old <- deprecatedHome::gen
    new <- deprecatedCore::gen
    child <- deprecatedUser::Child(value = 2)
    saved <- deprecatedUser::saved
    stopifnot(
      identical(suppressWarnings(deprecatedHome::shout("hi")), "HI"),
      identical(new(1), "double"),
      identical(new("x"), "character"),
      identical(suppressWarnings(old(1)), "double"),
      identical(suppressWarnings(old("x")), "character"),
      S7::S7_inherits(child, deprecatedHome::Foo),
      S7::S7_inherits(saved, deprecatedHome::Foo),
      !S7::S7_inherits(saved, deprecatedHome::Bar),
      identical(S7::prop(deprecatedHome::Foo, "name"), "Foo"),
      identical(S7::prop(deprecatedHome::Bar, "name"), "Bar"),
      identical(new(child), 2),
      identical(new(saved), 3),
      identical(suppressWarnings(old(child)), 2),
      identical(suppressWarnings(old(saved)), 3),
      identical(
        S7::method(old, S7::class_double),
        S7::method(new, S7::class_double)
      ),
      identical(S7::S7_methods(old), S7::S7_methods(new))
    )
    S7::method(old, S7::class_logical) <- function(x, ...) "logical"
    stopifnot(identical(new(TRUE), "logical"))
    S7::method(old, S7::class_logical) <- NULL
    stopifnot(nrow(S7::S7_methods(new)) == 3L)
    S7::method(new, deprecatedHome::Bar) <- \(x, ...) -S7::prop(x, "value")
    stopifnot(
      identical(new(deprecatedHome::Bar(value = 4)), -4),
      identical(new(saved), 3)
    )
    TRUE
  }
  expect_identical(callr::r(check, libpath = .libPaths()), TRUE)

  quick_install(file.path(fixtures, "user"), lib)
  expect_identical(callr::r(check, libpath = .libPaths()), TRUE)
})

test_that("deprecated_generic() validates its inputs", {
  new_gen := new_generic("x")
  expect_snapshot(error = TRUE, {
    deprecated_generic(1, new = new_gen, when = "1.0.0")
    deprecated_generic("old_gen", new = new_gen)
    deprecated_generic("old_gen", new = new_gen, when = "next year")
    deprecated_generic("old_gen", new = mean, when = "1.0.0")
    deprecated_generic("old_gen", when = "1.0.0")
    deprecated_generic("old_gen", dispatch_args = 1, when = "1.0.0")
    deprecated_generic("old_gen", "x", \(x) x, when = "1.0.0")
    deprecated_generic("old_gen", "x", new = new_gen, when = "1.0.0")
    deprecated_generic(
      "old_gen",
      fun = \(x) S7_dispatch(),
      new = new_gen,
      when = "1.0.0"
    )
    deprecated_generic("old_gen", new = new_gen, when = "1.0.0", new_label = 1)
    deprecated_generic("old_gen", new = new_gen, when = "1.0.0", new_label = "")
    deprecated_generic(
      "old_gen",
      "x",
      when = "1.0.0",
      new_label = "x()"
    )
    deprecated_generic(
      "old_gen",
      new = new_gen,
      when = "1.0.0",
      method = "warn"
    )
  })
})

test_that("deprecated_class() warns without changing the class", {
  Pet := new_class(properties = list(name = class_character))
  Dog := deprecated_class(
    properties = list(name = class_character),
    new = Pet,
    when = "2.0.0"
  )

  expect_snapshot(d <- Dog(name = "Fido"))
  expect_identical(Dog@name, "Dog")
  expect_identical(S7_class(d), as_class(Dog))
  expect_identical(S7_class(d)@name, "Dog")
  expect_identical(d@name, "Fido")
  expect_identical(S7_inherits(d, Pet), FALSE)
  expect_identical(formals(Dog), formals(Pet))
  expect_snapshot(expect_no_warning(print(d)))
})

test_that("deprecated classes keep their own methods and subclasses", {
  Dog := new_class(properties = list(name = class_character))
  Puppy := new_class(parent = Dog)
  saved_path <- withr::local_tempfile()
  saveRDS(Puppy(name = "Fido"), saved_path)
  saved <- readRDS(saved_path)

  Pet := new_class(properties = list(name = class_character))
  Dog := deprecated_class(
    properties = list(name = class_character),
    new = Pet,
    when = "2.0.0"
  )

  speak := new_generic("x")
  method(speak, Dog) <- function(x) paste("Woof!", x@name)
  expect_equal(speak(saved), "Woof! Fido")
  expect_no_warning(puppy <- Puppy(name = "Rex"))
  expect_equal(speak(puppy), "Woof! Rex")
  expect_identical(S7_inherits(saved, Dog), TRUE)
  expect_snapshot(error = TRUE, speak(Pet(name = "Rex")))

  BigDog := new_class(parent = Dog)
  expect_identical(BigDog@parent, as_class(Dog))
  expect_no_warning(big <- BigDog(name = "Rex"))
  expect_equal(speak(big), "Woof! Rex")

  method(speak, Dog) <- NULL
  method(speak, Dog | Pet) <- function(x) x@name
  expect_equal(speak(Pet(name = "Rex")), "Rex")
  expect_equal(speak(saved), "Fido")
})

test_that("deprecated_class() without a replacement still constructs", {
  Cat := deprecated_class(
    properties = list(lives = class_double),
    when = "3.0.0"
  )

  expect_snapshot(felix <- Cat(lives = 9))
  expect_equal(felix@lives, 9)
  expect_equal(S7_class(felix)@name, "Cat")
})

test_that("replacement labels can preserve generic identities", {
  Foo := new_class(properties = list(x = class_double))
  Child := new_class(parent = Foo)
  foo := new_generic("x")
  method(foo, Foo) <- function(x) x@x
  bar <- foo
  foo := deprecated_generic(new = bar, when = "2.0.0", new_label = "bar()")

  x <- Foo(x = 1)
  expect_no_warning(child <- Child(x = 2))
  expect_equal(bar(child), 2)
  expect_snapshot(out <- foo(x))
  expect_equal(out, 1)
  expect_snapshot(print(foo))
  older := deprecated_generic(new = foo, when = "3.0.0", new_label = "bar()")
  expect_snapshot(print(older))
})

test_that("replacement labels work with lifecycle", {
  skip_if_not_installed("lifecycle")
  foo := new_generic("x")
  bar <- foo
  foo := deprecated_generic(
    new = bar,
    when = "2.0.0",
    new_label = "bar()",
    method = "lifecycle(stop)"
  )
  expect_snapshot(error = TRUE, foo(1))
})

test_that("deprecated classes preserve constructor scope and validation", {
  skip_if_not_installed("lifecycle")
  default_name <- "Fido"
  Pet := new_class(properties = list(name = class_character))
  Dog := deprecated_class(
    properties = list(name = class_character),
    constructor = function(name = default_name) {
      new_object(S7_object(), name = name)
    },
    validator = function(self) {
      if (length(self@name) != 1L) "name must have length 1"
    },
    new = Pet,
    when = "2.0.0",
    method = "lifecycle(stop)"
  )

  expect_snapshot(error = TRUE, Dog())
  Puppy := new_class(parent = Dog)
  expect_no_warning(puppy <- Puppy())
  expect_identical(puppy@name, "Fido")
  expect_snapshot(error = TRUE, Puppy(name = character()))
})

test_that("deprecated classes construct property defaults silently", {
  dep := local_package({
    Dog := deprecated_class(when = "1.0.0")
    Holder := new_class(properties = list(dog = Dog))
  })
  expect_no_warning(holder <- dep$Holder())
  expect_identical(S7_inherits(holder@dog, dep$Dog), TRUE)

  expect_no_warning(Holder := new_class(properties = list(dog = dep$Dog)))
  expect_no_warning(holder <- Holder())
  expect_identical(S7_inherits(holder@dog, dep$Dog), TRUE)
})

test_that("installed direct property defaults need rebuilding after deprecation", {
  skip_if(quick_test())
  skip_if_not_installed("lifecycle")
  lib <- local_libpath()
  fixtures <- test_path("deprecated")

  check <- function(deprecated, rebuilt = FALSE) {
    for (policy in c("Base", "Warn", "Stop")) {
      for (kind in c("Box", "External", "Explicit")) {
        name <- paste0(policy, kind)
        # Each call gets fresh lifecycle state, including indirect warnings.
        out <- callr::r(
          function(name) {
            options(cli.unicode = FALSE, lifecycle_verbosity = "warning")
            constructor <- getExportedValue("deprecatedDefaultsUser", name)
            warnings <- character()
            value <- tryCatch(
              withCallingHandlers(constructor(), warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
              }),
              error = identity
            )
            if (inherits(value, "error")) {
              list(
                error = conditionMessage(value),
                error_class = class(value)[[1L]],
                warnings = warnings
              )
            } else {
              list(
                value = list(
                  name = S7::S7_class(value@item)@name,
                  package = S7::S7_class(value@item)@package,
                  size = value@item@size
                ),
                warnings = warnings
              )
            }
          },
          args = list(name),
          libpath = .libPaths()
        )
        signals <- deprecated &&
          (kind == "Explicit" || (kind == "Box" && !rebuilt))
        stops <- signals && policy == "Stop"
        expect_identical(
          out$value,
          if (!stops) {
            list(name = policy, package = "deprecatedDefaults", size = 7)
          }
        )
        expect_length(out$warnings, as.integer(signals && !stops))
        expect_identical(
          out$error_class,
          if (stops) "lifecycle_error_deprecated"
        )
        if (signals) {
          expect_snapshot(cat(c(name, out$warnings, out$error), sep = "\n"))
        }
      }
    }
  }

  quick_install(file.path(fixtures, c("defaults-v1", "defaults-user")), lib)
  check(deprecated = FALSE)
  quick_install(file.path(fixtures, "defaults-v2"), lib)
  check(deprecated = TRUE)
  quick_install(file.path(fixtures, "defaults-user"), lib)
  check(deprecated = TRUE, rebuilt = TRUE)
})

test_that("deprecated classes name replacements from other packages", {
  dep := local_package({
    Pet := new_class()
  })
  Dog := deprecated_class(new = dep$Pet, when = "2.0.0")
  expect_snapshot(invisible(Dog()))
})

test_that("deprecated classes preserve S4 parents", {
  local_S4_classes()
  S4Parent <- methods::setClass(
    "DeprecatedS4Parent",
    slots = c(value = "numeric")
  )
  Old := deprecated_class(parent = S4Parent, when = "1.0.0", package = NULL)

  expect_snapshot(obj <- Old(value = 1))
  expect_identical(obj@value, 1)
  expect_identical(methods::validObject(obj, test = TRUE), TRUE)
  Child := new_class(parent = Old, package = NULL)
  expect_no_warning(child <- Child(value = 2))
  expect_identical(child@value, 2)
})

test_that("deprecated classes work with the union operator", {
  Dog := deprecated_class(when = "1.0.0")
  Cat := deprecated_class(when = "1.0.0")

  expect_identical(Dog | NULL, as_class(Dog) | NULL)
  expect_identical(NULL | Dog, NULL | as_class(Dog))
  expect_identical(Dog | Cat, as_class(Dog) | as_class(Cat))
  expect_identical(Dog | class_double, as_class(Dog) | class_double)
})

test_that("deprecated_class() validates its inputs", {
  expect_snapshot(error = TRUE, {
    deprecated_class(name = 1, when = "1.0.0")
    deprecated_class(name = "Old")
    deprecated_class(name = "Old", when = "next year")
    deprecated_class(name = "Old", when = "1.0.0", method = "warn")
    deprecated_class(name = "Old", new = 1, when = "1.0.0")
    deprecated_class(name = "Old", new = class_double, when = "1.0.0")
  })
})

test_that("deprecated_property() with a replacement delegates and warns", {
  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property("count", new = "size", when = "1.5.0")
    )
  )

  b <- Basket(size = 3)
  expect_snapshot({
    print(b@count)
    b@count <- 5
  })
  expect_equal(b@size, 5)
})

test_that("deprecated_property() only warns at construction when actually used", {
  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property("count", new = "size", when = "1.5.0")
    )
  )

  expect_no_warning(Basket(size = 3))
  expect_snapshot(b <- Basket(count = 7))
  expect_equal(b@size, 7)
})

test_that("deprecated_property() without a replacement still stores data", {
  Hat := new_class(
    properties = list(
      deprecated_property("brim", when = "0.9.0", class = class_double)
    )
  )

  expect_no_warning(h <- Hat(brim = 2))
  expect_snapshot({
    print(h@brim)
    h@brim <- 3
  })
  expect_equal(attr(h, "brim"), 3)
})

test_that("deprecated_property() without a replacement validates stored values", {
  Hat := new_class(
    properties = list(
      deprecated_property("brim", when = "1.0.0", class = class_double)
    )
  )

  h <- Hat(brim = 2)
  expect_snapshot(error = TRUE, Hat(brim = "invalid"))
  expect_snapshot(error = TRUE, h@brim <- "invalid")
  expect_identical(attr(h, "brim"), 2)
})

test_that("deprecated_property() without a replacement preserves NULL", {
  Hat := new_class(
    properties = list(
      deprecated_property("brim", when = "1.0.0", class = NULL | class_double)
    )
  )
  expect_no_warning(h <- Hat(brim = NULL))
  expect_snapshot(value <- h@brim)
  expect_null(value)
  expect_snapshot(h@brim <- 2)
  expect_identical(attr(h, "brim"), 2)
})

test_that("retired properties preserve validators", {
  Hat := new_class(
    properties = list(
      deprecated_property(
        "brim",
        when = "1.0.0",
        class = class_double,
        default = 1,
        validator = function(value) {
          if (value < 0) "must be non-negative"
        }
      )
    )
  )
  expect_no_warning(h <- Hat())
  expect_identical(attr(h, "brim"), 1)
  expect_snapshot(error = TRUE, Hat(brim = -1))
  expect_snapshot(error = TRUE, h@brim <- -1)
  expect_identical(attr(h, "brim"), 1)
  expect_snapshot(h@brim <- 2)
  expect_identical(attr(h, "brim"), 2)
})

test_that("renamed properties reject validators", {
  expect_snapshot(
    error = TRUE,
    deprecated_property(
      "count",
      new = "size",
      when = "1.0.0",
      validator = \(value) if (value < 0) "must be non-negative"
    )
  )
})

test_that("equal property values are silent even with stopping deprecations", {
  skip_if_not_installed("lifecycle")
  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property(
        "count",
        new = "size",
        when = "1.0.0",
        method = "lifecycle(stop)"
      ),
      deprecated_property(
        "retired",
        class = class_double,
        when = "1.0.0",
        method = "lifecycle(stop)"
      )
    )
  )
  expect_no_warning(b <- Basket(size = 2, count = 2, retired = 3))
  expect_no_warning(b@count <- 2)
  expect_no_warning(b@retired <- 3)
  expect_snapshot(error = TRUE, b@count <- 4)
  expect_snapshot(error = TRUE, b@retired <- 4)
  expect_snapshot(error = TRUE, Basket(size = 2, count = 4))
})

test_that("deprecated properties are omitted from object printing", {
  Basket := new_class(
    properties = list(
      size = class_double,
      doubled = new_property(getter = \(self) self@size * 2),
      deprecated_property("count", new = "size", when = "1.5.0"),
      deprecated_property("retired", when = "1.5.0", class = class_double)
    )
  )
  b <- Basket(size = 3, retired = 1)

  expect_snapshot({
    expect_no_warning(print(b))
    expect_no_warning(str(b))
    expect_no_warning(str(list(b)))
  })
  expect_snapshot({
    b@count
    b@retired
  })
})

test_that("printing respects inherited and overridden deprecated properties", {
  Parent := new_class(
    properties = list(
      deprecated_property("x", when = "1.0.0", class = class_double)
    )
  )
  Child := new_class(parent = Parent)
  Visible := new_class(parent = Parent, properties = list(x = class_double))

  expect_snapshot({
    expect_no_warning(print(Child()))
    expect_no_warning(print(Visible(x = 1)))
  })
})

test_that("printing skips properties that signal deprecation errors", {
  skip_if_not_installed("lifecycle")

  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property(
        "count",
        new = "size",
        when = "1.5.0",
        method = "lifecycle(stop)"
      )
    )
  )
  b <- Basket(size = 3)

  expect_snapshot({
    print(b)
    str(b)
  })
  expect_snapshot(b@count, error = TRUE)
})

test_that("deprecated_property() validates its inputs", {
  expect_snapshot(error = TRUE, {
    deprecated_property(1, when = "1.0.0")
    deprecated_property("count", new = 1, when = "1.0.0")
    deprecated_property("count", new = "size")
    deprecated_property("count", new = "size", when = "next year")
    deprecated_property(
      "count",
      new = "size",
      when = "1.0.0",
      method = "warn"
    )
  })
})

test_that("deprecation warnings mention the package", {
  pkg <- local_package("pkgA", {
    new_gen := new_generic("x")
    method(new_gen, class_double) <- function(x) x
    old_gen := deprecated_generic(new = new_gen, when = "1.1.0")
  })

  expect_snapshot(invisible(pkg$old_gen(1)))
})

test_that("method = 'lifecycle(warn)' signals with lifecycle", {
  skip_if_not_installed("lifecycle")

  pkg <- local_package("pkgA", {
    new_gen := new_generic("x")
    method(new_gen, class_double) <- function(x) x
    old_gen := deprecated_generic(
      new = new_gen,
      when = "1.1.0",
      method = "lifecycle(warn)"
    )
  })

  expect_snapshot(invisible(pkg$old_gen(1)))
})

test_that("method = 'lifecycle(stop)' errors", {
  skip_if_not_installed("lifecycle")

  new_gen := new_generic("x")
  method(new_gen, class_double) <- function(x) x
  old_gen := deprecated_generic(
    new = new_gen,
    when = "1.1.0",
    method = "lifecycle(stop)"
  )

  expect_snapshot(old_gen(1), error = TRUE)
})

test_that("deprecated_property() works with lifecycle", {
  skip_if_not_installed("lifecycle")

  Basket := new_class(
    properties = list(
      size = class_double,
      deprecated_property(
        "count",
        new = "size",
        when = "1.5.0",
        method = "lifecycle(warn)"
      )
    )
  )
  b <- Basket(size = 3)

  expect_snapshot(invisible(b@count))
})

test_that("props() attributes repeated lifecycle warnings to its direct caller", {
  skip_if_not_installed("lifecycle")
  withr::local_options(lifecycle_verbosity = "warning")

  directProps := local_package({
    Renamed := new_class(
      properties = list(
        size = class_double,
        deprecated_property(
          "count",
          new = "size",
          when = "2.0.0",
          method = "lifecycle(warn)"
        )
      )
    )
    Retired := new_class(
      properties = list(
        deprecated_property(
          "item",
          class = class_double,
          when = "2.0.0",
          method = "lifecycle(warn)"
        )
      )
    )
  })

  for (x in list(
    directProps$Renamed(size = 1),
    directProps$Retired(item = 2)
  )) {
    warnings <- list()
    expect_snapshot({
      withCallingHandlers(
        {
          invisible(props(x))
          invisible(props(x))
        },
        lifecycle_warning_deprecated = function(w) {
          warnings[[length(warnings) + 1L]] <<- w
        }
      )
    })
    expect_length(warnings, 2L)
  }
})

test_that("generated constructors attribute property deprecation to their caller", {
  skip_if_not_installed("lifecycle")
  withr::local_options(lifecycle_verbosity = "warning")
  constructorProps := local_package({
    Renamed := new_class(
      properties = list(
        size = class_double,
        deprecated_property(
          "count",
          new = "size",
          when = "2.0.0",
          method = "lifecycle(warn)"
        )
      )
    )
  })

  warnings <- list()
  expect_snapshot({
    withCallingHandlers(
      {
        invisible(constructorProps$Renamed(size = 1, count = 2))
        invisible(constructorProps$Renamed(size = 1, count = 3))
      },
      lifecycle_warning_deprecated = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
      }
    )
  })
  expect_length(warnings, 2L)
})

test_that("props() attributes indirect lifecycle warnings to the downstream package", {
  skip_if(quick_test())
  skip_if_not_installed("lifecycle")

  for (verbosity in c("default", "warning")) {
    out <- callr::r(
      function(local_package, verbosity) {
        library(S7)
        environment(local_package) <- asNamespace("S7")
        options(cli.unicode = FALSE, lifecycle_verbosity = verbosity)
        indirectProps := local_package({
          Renamed := new_class(
            properties = list(
              size = class_double,
              deprecated_property(
                "count",
                new = "size",
                when = "2.0.0",
                method = "lifecycle(warn)"
              )
            )
          )
          Retired := new_class(
            properties = list(
              deprecated_property(
                "item",
                class = class_double,
                when = "2.0.0",
                method = "lifecycle(warn)"
              )
            )
          )
        })
        propsUser := local_package({
          read_props <- function(x) props(x)
        })
        capture <- function(expr) {
          warnings <- character()
          value <- withCallingHandlers(force(expr), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          })
          list(value = value, warnings = warnings)
        }
        lapply(
          list(
            indirectProps$Renamed(size = 1),
            indirectProps$Retired(item = 2)
          ),
          function(x) {
            list(
              first = capture(propsUser$read_props(x)),
              second = capture(propsUser$read_props(x)),
              direct = capture(props(x))
            )
          }
        )
      },
      args = list(local_package = local_package, verbosity = verbosity),
      libpath = .libPaths()
    )
    for (result in out) {
      expect_length(result$first$warnings, 1L)
      expect_length(result$second$warnings, 0L)
      expect_length(result$direct$warnings, as.integer(verbosity == "warning"))
      expect_identical(result$first$value, result$second$value)
      expect_identical(result$first$value, result$direct$value)
      expect_snapshot(cat(c(verbosity, result$first$warnings), sep = "\n"))
    }
  }
})

test_that("deprecated generics and classes print nicely", {
  new_gen := new_generic("x")
  old_gen := deprecated_generic(new = new_gen, when = "1.1.0")
  Pet := new_class()
  Dog := deprecated_class(new = Pet, when = "2.0.0")
  Cat := deprecated_class(when = "3.0.0")

  expect_snapshot({
    print(old_gen)
    print(Dog)
    print(Cat)
  })
})
