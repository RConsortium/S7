# Scenario definitions for the evolution compat lab. See run.R and README.md.
#
# There is one scenario per "Changing a ..." section of
# `vignette("evolution")`, named after the section it verifies. Where a
# section describes several transitions (e.g. a bare change versus the
# recommended deprecation path), the scenario packs each variant into evoA
# as a separate generic or class, and the smoke test asserts the expected
# outcome for each, conditional on the installed evoA version and on whether
# evoB was rebuilt against it.
#
# Each scenario describes an upstream package evoA at version 1.0.0 (`a_v1`)
# and 2.0.0 (`a_v2`), a downstream package evoB (`b`) written against 1.0.0,
# and a smoke test (`b_test`) that must pass against 1.0.0. What we learn is
# how (and *when*) each scenario fails against 2.0.0.
#
# Code is given unquoted and deparsed into R files in the fixture packages,
# so it follows user-facing S7 style, not S7-internal style.
#
# `b_ns` gives extra NAMESPACE directives for evoB: registering a method on
# another package's generic requires importing the generic, since
# `method(evoA::gen, ...) <- f` is a replacement call and can't assign to
# `evoA::gen`.
#
# `a_core` gives code for a third package, evoACore, that exists only at
# version 2.0.0 (it's installed just before evoA 2.0.0, which imports it).
# Use it to model evoA moving a generic or class to a lower-level package.
# `a_ns` gives extra NAMESPACE directives for evoA at 2.0.0.
scenario <- function(
  name,
  expect,
  a_v1,
  a_v2,
  b,
  b_test,
  b_ns = NULL,
  a_core = NULL,
  a_ns = NULL
) {
  a_core <- substitute(a_core)
  list(
    name = name,
    expect = expect,
    a_v1 = deparse_code(substitute(a_v1)),
    a_v2 = deparse_code(substitute(a_v2)),
    b = deparse_code(substitute(b)),
    b_test = deparse_code(substitute(b_test)),
    b_ns = b_ns,
    a_core = if (!is.null(a_core)) deparse_code(a_core),
    a_ns = a_ns
  )
}

# Deparse a `{` block into the lines of its body
deparse_code <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], quote(`{`))) {
    exprs <- as.list(expr)[-1]
  } else {
    exprs <- list(expr)
  }
  unlist(lapply(exprs, deparse, width.cutoff = 80L))
}

# Shared smoke-test idioms, defined inline in each `b_test` that needs them:
#
#   v2 <- packageVersion("evoA") >= "2.0.0"
#   evoB:::built_against == "2.0.0"  # TRUE once evoB is rebuilt (needs
#                                    # `built_against` recorded in `b`)
#
#   expect_warning(expr): returns the value, asserting it signaled a warning
#   expect_error(expr, pattern): asserts an error matching `pattern`
#   expect_silent(expr): returns the value, failing on any warning

scenarios <- list(
  # Changing dispatch ------------------------------------------------------

  scenario(
    name = "gen-change-dispatch",
    expect = paste(
      "Changing a generic's dispatch arguments breaks every downstream",
      "method, with no workaround: dispatch arguments are the generic's",
      "identity. evoA makes two such changes: gen_single gains a second",
      "dispatch argument and gen_nodots (which has no `...`) gains an",
      "ordinary argument. A stale evoB still loads and dispatches",
      "gen_single, but calls to gen_nodots fail. Reinstalling evoB fails",
      "outright, because multidispatch registration requires a list",
      "signature. The only safe path is a new generic name plus",
      "deprecation (see gen-rename)."
    ),
    a_v1 = {
      gen_single := new_generic("x")
      gen_nodots := new_generic("x", fun = function(x) S7_dispatch())
    },
    a_v2 = {
      gen_single := new_generic(c("x", "y"))
      gen_nodots := new_generic(
        "x",
        fun = function(x, verbose = FALSE) S7_dispatch()
      )
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen_single, BClass) <- function(x, ...) paste0("B:", x@val)
      method(gen_nodots, BClass) <- function(x) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen_single(x), "B:1"))
      stopifnot(identical(evoA::gen_nodots(x), "B:1"))
    },
    b_ns = c("importFrom(evoA, gen_single)", "importFrom(evoA, gen_nodots)")
  ),

  # Adding an argument -----------------------------------------------------

  scenario(
    name = "gen-add-arg",
    expect = paste(
      "A adds an optional argument to two generics that have `...`. B's",
      "method on gen lacks the argument; its method on gen_fixed already",
      "has it (the recommended transition, possible because methods may",
      "have arguments the generic lacks). Installation, loading, and calls",
      "remain silent throughout; R CMD check of B reports the gen mismatch",
      "as a single NOTE."
    ),
    a_v1 = {
      gen := new_generic("x")
      gen_fixed := new_generic("x")
    },
    a_v2 = {
      gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
        S7_dispatch()
      })
      gen_fixed := new_generic("x", fun = function(x, verbose = FALSE, ...) {
        S7_dispatch()
      })
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, ...) paste0("B:", x@val)
      method(gen_fixed, BClass) <- function(x, verbose = FALSE, ...) {
        paste0("B:", x@val, if (verbose) "!")
      }
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
      stopifnot(identical(evoA::gen_fixed(x), "B:1"))
    },
    b_ns = c("importFrom(evoA, gen)", "importFrom(evoA, gen_fixed)")
  ),

  # Removing an argument ---------------------------------------------------

  scenario(
    name = "gen-remove-arg",
    expect = paste(
      "A removes an optional argument from gen; B's method still has it.",
      "This is silent in both directions (methods may have extra",
      "arguments), so B can drop the argument at leisure. A also keeps the",
      "argument in gen_deprecated but warns when it is supplied (the",
      "recommended transition for callers); B's method no longer uses it,",
      "which R CMD check reports as a NOTE until A finishes the removal."
    ),
    a_v1 = {
      gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
        S7_dispatch()
      })
      gen_deprecated := new_generic(
        "x",
        fun = function(x, verbose = FALSE, ...) S7_dispatch()
      )
    },
    a_v2 = {
      gen := new_generic("x")
      gen_deprecated := new_generic(
        "x",
        fun = function(x, verbose = FALSE, ...) {
          if (!missing(verbose)) {
            warning("`verbose` is deprecated and will be ignored")
          }
          S7_dispatch()
        }
      )
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, verbose = FALSE, ...) {
        paste0("B:", x@val)
      }
      method(gen_deprecated, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
      stopifnot(identical(evoA::gen_deprecated(x), "B:1"))
      if (packageVersion("evoA") >= "2.0.0") {
        warned <- FALSE
        value <- withCallingHandlers(
          evoA::gen_deprecated(x, verbose = TRUE),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned, identical(value, "B:1"))
      }
    },
    b_ns = c("importFrom(evoA, gen)", "importFrom(evoA, gen_deprecated)")
  ),

  # Changing a default -----------------------------------------------------

  scenario(
    name = "gen-change-default",
    expect = paste(
      "A changes the default of a non-dispatch argument; B's method keeps",
      "the old default. Ordinary installation and loading are silent; R CMD",
      "check reports a NOTE. Dispatch never uses the method's default, so",
      "calls receive the generic's new default."
    ),
    a_v1 = {
      gen := new_generic("x", fun = function(x, drop = FALSE, ...) {
        S7_dispatch()
      })
    },
    a_v2 = {
      gen := new_generic("x", fun = function(x, drop = TRUE, ...) {
        S7_dispatch()
      })
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, drop = FALSE, ...) {
        paste0("B:", x@val, ":", drop)
      }
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      # The generic's default wins, whichever version is installed
      v2 <- packageVersion("evoA") >= "2.0.0"
      expected <- if (v2) "B:1:TRUE" else "B:1:FALSE"
      stopifnot(identical(evoA::gen(x), expected))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  # Renaming a generic -----------------------------------------------------

  scenario(
    name = "gen-rename",
    expect = paste(
      "A renames four generics with increasing levels of care. gen_plain",
      "simply disappears: callers see a run-time error, and a package that",
      "imported it would fail to install. gen_dep keeps the old name as a",
      "deprecated_generic() alias: registration and calls keep working",
      "through both names, warning through the old one. gen_ext is the same",
      "transition with B registering through a deferred",
      "new_external_generic(). gen_label renames the export instead, using",
      "new_label so the warning recommends the new spelling while both",
      "exports share B's methods."
    ),
    a_v1 = {
      gen_plain := new_generic("x")
      method(gen_plain, class_any) <- function(x, ...) "A"
      gen_dep := new_generic("x")
      gen_ext := new_generic("x")
      gen_label := new_generic("x")
    },
    a_v2 = {
      gen_new := new_generic("x")
      gen_dep2 := new_generic("x")
      gen_dep := deprecated_generic(new = gen_dep2, when = "2.0.0")
      gen_ext2 := new_generic("x")
      gen_ext := deprecated_generic(new = gen_ext2, when = "2.0.0")
      gen_label := new_generic("x")
      gen_label2 <- gen_label
      gen_label := deprecated_generic(
        new = gen_label2,
        when = "2.0.0",
        new_label = "gen_label2()"
      )
    },
    b = {
      BClass := new_class()
      gen_ext := new_external_generic(package = "evoA", dispatch_args = "x")
      method(gen_dep, BClass) <- function(x, ...) "B"
      method(gen_ext, BClass) <- function(x, ...) "B"
      method(gen_label, BClass) <- function(x, ...) "B"
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass()
      if (packageVersion("evoA") >= "2.0.0") {
        # A bare rename removes the export entirely
        stopifnot(!"gen_plain" %in% getNamespaceExports("evoA"))
        err <- tryCatch(evoA::gen_plain(x), error = identity)
        stopifnot(
          inherits(err, "error"),
          grepl("gen_plain", conditionMessage(err), fixed = TRUE)
        )
        # Deprecated aliases warn through the old name and dispatch through
        # both names
        warned <- FALSE
        value <- withCallingHandlers(
          evoA::gen_dep(x),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned, identical(value, "B"))
        stopifnot(identical(evoA::gen_dep2(x), "B"))
        warned <- FALSE
        value <- withCallingHandlers(
          evoA::gen_ext(x),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned, identical(value, "B"))
        stopifnot(identical(evoA::gen_ext2(x), "B"))
        # new_label points at the new spelling
        message <- NULL
        value <- withCallingHandlers(
          evoA::gen_label(x),
          warning = function(w) {
            message <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(
          identical(value, "B"),
          grepl("gen_label2()", message, fixed = TRUE),
          identical(evoA::gen_label2(x), "B")
        )
      } else {
        stopifnot(
          identical(evoA::gen_plain(x), "A"),
          identical(evoA::gen_dep(x), "B"),
          identical(evoA::gen_ext(x), "B"),
          identical(evoA::gen_label(x), "B")
        )
      }
    },
    b_ns = c("importFrom(evoA, gen_dep)", "importFrom(evoA, gen_label)")
  ),

  # Removing a generic -----------------------------------------------------

  scenario(
    name = "gen-remove",
    expect = paste(
      "A retires a generic without replacement using deprecated_generic(),",
      "keeping its dispatch arguments and custom function. B's method",
      "registration survives both an upstream-only upgrade and a rebuild;",
      "calls warn and still dispatch, and the custom function keeps its",
      "lexical scope and default argument."
    ),
    a_v1 = {
      prefix <- "A:"
      gen := new_generic("x", fun = function(x, ending = "!", ...) {
        out <- S7_dispatch()
        paste0(prefix, out, ending)
      })
    },
    a_v2 = {
      prefix <- "A:"
      gen := deprecated_generic(
        "x",
        fun = function(x, ending = "!", ...) {
          out <- S7_dispatch()
          paste0(prefix, out, ending)
        },
        when = "2.0.0"
      )
    },
    b = {
      BClass := new_class()
      method(gen, BClass) <- function(x, ending = "!", ...) "B"
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass()
      v2 <- packageVersion("evoA") >= "2.0.0"
      call <- function(...) {
        if (v2) {
          warned <- FALSE
          value <- withCallingHandlers(
            evoA::gen(...),
            warning = function(w) {
              warned <<- TRUE
              invokeRestart("muffleWarning")
            }
          )
          stopifnot(warned)
          value
        } else {
          evoA::gen(...)
        }
      }
      stopifnot(
        is.function(S7::method(evoA::gen, evoB:::BClass)),
        identical(call(x), "A:B!"),
        identical(call(x, ending = "?"), "A:B?")
      )
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  # Moving a generic to another package ------------------------------------

  scenario(
    name = "gen-move-package",
    expect = paste(
      "A moves three generics to evoACore. gen_rex is re-exported through",
      "the NAMESPACE: registration follows the generic to its new home and",
      "calls through both packages work. gen_copy is re-exported by",
      "assignment (`gen_copy <- evoACore::gen_copy`, the anti-pattern the",
      "vignette warns against): a stale B keeps working because loading",
      "re-registers into the copy, but rebuilding registers into",
      "evoACore's original, so calls through evoA's empty copy fail while",
      "evoACore dispatches fine.",
      "gen_wrap retires the old home with a deprecated_generic() alias:",
      "calls through evoA warn and reach the same methods."
    ),
    a_v1 = {
      gen_rex := new_generic("x")
      gen_copy := new_generic("x")
      gen_wrap := new_generic("x")
    },
    a_core = {
      gen_rex := new_generic("x")
      gen_copy := new_generic("x")
      gen_wrap := new_generic("x")
    },
    a_v2 = {
      gen_copy <- evoACore::gen_copy
      gen_wrap := deprecated_generic(new = evoACore::gen_wrap, when = "2.0.0")
    },
    a_ns = c("importFrom(evoACore, gen_rex)", "export(gen_rex)"),
    b = {
      built_against <- as.character(getNamespaceVersion("evoA"))
      BClass := new_class()
      method(gen_rex, BClass) <- function(x, ...) "B"
      method(gen_copy, BClass) <- function(x, ...) "B"
      method(gen_wrap, BClass) <- function(x, ...) "B"
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass()
      if (packageVersion("evoA") >= "2.0.0") {
        stopifnot(
          identical(evoA::gen_rex(x), "B"),
          identical(evoACore::gen_rex(x), "B")
        )
        if (evoB:::built_against == "2.0.0") {
          # Registration followed the generic's home package, so the
          # binding copy in evoA has its own, empty method table
          err <- tryCatch(evoA::gen_copy(x), error = identity)
          stopifnot(
            inherits(err, "error"),
            grepl("Can't find method", conditionMessage(err), fixed = TRUE),
            identical(evoACore::gen_copy(x), "B")
          )
        } else {
          # A stale B re-registers into the copy on load, so calls through
          # evoA still work
          stopifnot(identical(evoA::gen_copy(x), "B"))
        }
        warned <- FALSE
        value <- withCallingHandlers(
          evoA::gen_wrap(x),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(
          warned,
          identical(value, "B"),
          identical(evoACore::gen_wrap(x), "B")
        )
      } else {
        stopifnot(
          identical(evoA::gen_rex(x), "B"),
          identical(evoA::gen_copy(x), "B"),
          identical(evoA::gen_wrap(x), "B")
        )
      }
    },
    b_ns = c(
      "importFrom(evoA, gen_rex)",
      "importFrom(evoA, gen_copy)",
      "importFrom(evoA, gen_wrap)"
    )
  ),

  # Adding a property ------------------------------------------------------

  scenario(
    name = "class-add-prop",
    expect = paste(
      "A adds a property to two classes. Foo's new property doesn't clash",
      "with B's subclass, so everything works, stale or rebuilt. FooClash",
      "gains a property whose name B would use with an incompatible type:",
      "defining that subclass errors because the override doesn't narrow",
      "the parent property, which is why a real downstream subclass fails",
      "to install."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
      FooClash := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Foo := new_class(
        properties = list(size = class_double, extra = class_character)
      )
      FooClash := new_class(
        properties = list(size = class_double, y = class_character)
      )
    },
    b = {
      Foo := new_external_class(package = "evoA")
      Baz := new_class(parent = Foo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(obj@size, 1), identical(obj@y, 2))
      subclass <- function() {
        S7::new_class(
          name = "ClashSub",
          parent = evoA::FooClash,
          properties = list(y = S7::class_double)
        )
      }
      if (packageVersion("evoA") >= "2.0.0") {
        stopifnot(identical(evoA::Foo(size = 1, extra = "a")@extra, "a"))
        err <- tryCatch(subclass(), error = identity)
        stopifnot(
          inherits(err, "error"),
          grepl("must narrow", conditionMessage(err), fixed = TRUE)
        )
      } else {
        subclass()
      }
    }
  ),

  # Removing a property ----------------------------------------------------

  scenario(
    name = "class-remove-prop",
    expect = paste(
      "A removes a property two ways. Foo loses count outright: B still",
      "installs, but Foo(count = ) and x@count fail at run time. FooDep",
      "replaces count with deprecated_property(), keeping its class,",
      "default, and validator: construction stays silent, reads and",
      "changed writes warn, invalid values still fail, and print()/str()",
      "omit the property. A stale subclass of FooDep retains the old",
      "stored property and stays silent; rebuilding adopts the",
      "deprecation."
    ),
    a_v1 = {
      validate_count <- function(value) {
        if (any(value < 0)) "must be non-negative"
      }
      Foo := new_class(
        properties = list(size = class_double, count = class_double)
      )
      FooDep := new_class(
        properties = list(
          count = new_property(
            class_double,
            default = 1,
            validator = validate_count
          )
        )
      )
    },
    a_v2 = {
      validate_count <- function(value) {
        if (any(value < 0)) "must be non-negative"
      }
      Foo := new_class(properties = list(size = class_double))
      FooDep := new_class(
        properties = list(
          deprecated_property(
            "count",
            when = "2.0.0",
            class = class_double,
            default = 1,
            validator = validate_count
          )
        )
      )
    },
    b = {
      built_against <- as.character(getNamespaceVersion("evoA"))
      Foo := new_external_class(package = "evoA")
      FooDep := new_external_class(package = "evoA")
      Baz := new_class(parent = Foo)
      BazDep := new_class(parent = FooDep)
    },
    b_test = {
      library(evoB)
      expect_warning <- function(expr) {
        warned <- FALSE
        value <- withCallingHandlers(
          force(expr),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned)
        value
      }
      expect_silent <- function(expr) {
        withCallingHandlers(force(expr), warning = function(w) stop(w))
      }
      expect_error <- function(expr, pattern) {
        err <- tryCatch(force(expr), error = identity)
        stopifnot(
          inherits(err, "error"),
          grepl(pattern, conditionMessage(err), fixed = TRUE)
        )
      }
      if (packageVersion("evoA") >= "2.0.0") {
        # Outright removal fails at run time, not install time
        expect_error(evoA::Foo(size = 1, count = 2), "unused argument")
        expect_error(evoA::Foo(size = 1)@count, "Property not found")
        # Deprecation keeps everything working
        x <- expect_silent(evoA::FooDep(count = 2))
        stopifnot(identical(expect_warning(x@count), 2))
        expect_warning(x@count <- 3)
        stopifnot(identical(expect_warning(x@count), 3))
        expect_silent(capture.output(print(x), str(x)))
        expect_error(evoA::FooDep(count = -1), "must be non-negative")
        # A stale subclass retains the old property definition; a rebuilt
        # one adopts the deprecation
        xb <- evoB:::BazDep(count = 2)
        if (evoB:::built_against == "2.0.0") {
          stopifnot(identical(expect_warning(xb@count), 2))
        } else {
          stopifnot(identical(xb@count, 2))
        }
      } else {
        stopifnot(identical(evoA::Foo(size = 1, count = 2)@count, 2))
        stopifnot(identical(evoA::FooDep(count = 3)@count, 3))
        stopifnot(identical(evoB:::BazDep(count = 2)@count, 2))
      }
    }
  ),

  # Renaming a property ----------------------------------------------------

  scenario(
    name = "class-rename-prop",
    expect = paste(
      "A renames count to size, moving validation to size and marking",
      "count with deprecated_property(new = ). The old constructor",
      "argument, reads, and changed writes warn and delegate; equal values",
      "stay silent; invalid values fail through either name, leaving the",
      "old value intact; print() and str() stay silent. A stale subclass",
      "retains the stored count property and fails validation until B is",
      "rebuilt."
    ),
    a_v1 = {
      validate <- function(value) {
        if (any(value < 0)) "must be non-negative"
      }
      Foo := new_class(
        properties = list(
          count = new_property(
            class_double,
            default = 7,
            validator = validate
          )
        )
      )
    },
    a_v2 = {
      validate <- function(value) {
        if (any(value < 0)) "must be non-negative"
      }
      Foo := new_class(
        properties = list(
          size = new_property(
            class_double,
            default = 7,
            validator = validate
          ),
          deprecated_property("count", new = "size", when = "2.0.0")
        )
      )
    },
    b = {
      built_against <- as.character(getNamespaceVersion("evoA"))
      Foo := new_external_class(package = "evoA")
      Baz := new_class(parent = Foo)
    },
    b_test = {
      library(evoB)
      expect_warning <- function(expr) {
        warned <- FALSE
        value <- withCallingHandlers(
          force(expr),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned)
        value
      }
      expect_silent <- function(expr) {
        withCallingHandlers(force(expr), warning = function(w) stop(w))
      }
      if (packageVersion("evoA") >= "2.0.0") {
        # The old constructor argument warns and delegates
        x <- expect_warning(evoA::Foo(count = 2))
        stopifnot(identical(x@size, 2))
        # Equal values stay silent
        expect_silent(evoA::Foo(size = 2, count = 2))
        # Reads and changed writes warn and delegate
        stopifnot(identical(expect_warning(x@count), 2))
        expect_warning(x@count <- 5)
        stopifnot(identical(x@size, 5))
        # Validation lives on size and applies through both names
        for (expr in list(
          quote(evoA::Foo(size = -1)),
          quote(evoA::Foo(count = -1)),
          quote(x@count <- -1)
        )) {
          err <- withCallingHandlers(
            tryCatch(eval(expr), error = identity),
            warning = function(w) invokeRestart("muffleWarning")
          )
          stopifnot(
            inherits(err, "error"),
            grepl(
              "must be non-negative",
              conditionMessage(err),
              fixed = TRUE
            )
          )
        }
        stopifnot(identical(x@size, 5))
        expect_silent(capture.output(print(x), str(x)))
        # A stale subclass retains the stored property and fails
        # validation; rebuilding adopts the rename
        if (evoB:::built_against == "2.0.0") {
          xb <- expect_warning(evoB:::Baz(count = 2))
          stopifnot(identical(xb@size, 2))
        } else {
          err <- withCallingHandlers(
            tryCatch(evoB:::Baz(count = 2), error = identity),
            warning = function(w) invokeRestart("muffleWarning")
          )
          stopifnot(inherits(err, "error"))
        }
      } else {
        x <- evoA::Foo(count = 2)
        stopifnot(identical(x@count, 2))
        x@count <- 3
        stopifnot(identical(x@count, 3))
        stopifnot(identical(evoB:::Baz(count = 2)@count, 2))
      }
    }
  ),

  # Changing a property's type or validator --------------------------------

  scenario(
    name = "class-change-prop-type",
    expect = paste(
      "A narrows FooNarrow's property type from numeric to integer: B",
      "still installs, but constructing an instance with a double fails at",
      "run time. FooWarn shows the recommended transition: keep the type",
      "and warn in the validator about values that will become invalid, so",
      "construction warns instead of failing."
    ),
    a_v1 = {
      FooNarrow := new_class(properties = list(size = class_numeric))
      FooWarn := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      FooNarrow := new_class(properties = list(size = class_integer))
      FooWarn := new_class(
        properties = list(size = class_double),
        validator = function(self) {
          if (self@size != trunc(self@size)) {
            warning(
              "@size should be a whole number; this will become an error"
            )
          }
          NULL
        }
      )
    },
    b = {
      FooNarrow := new_external_class(package = "evoA")
      Baz := new_class(parent = FooNarrow)
    },
    b_test = {
      library(evoB)
      if (packageVersion("evoA") >= "2.0.0") {
        for (expr in list(
          quote(evoA::FooNarrow(size = 1)),
          quote(evoB:::Baz(size = 1))
        )) {
          err <- tryCatch(eval(expr), error = identity)
          stopifnot(
            inherits(err, "error"),
            grepl("integer", conditionMessage(err), fixed = TRUE)
          )
        }
        stopifnot(identical(evoA::FooNarrow(size = 1L)@size, 1L))
        warned <- FALSE
        value <- withCallingHandlers(
          evoA::FooWarn(size = 3.5),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned, identical(value@size, 3.5))
        withCallingHandlers(
          evoA::FooWarn(size = 3),
          warning = function(w) stop(w)
        )
      } else {
        stopifnot(identical(evoA::FooNarrow(size = 1)@size, 1))
        stopifnot(identical(evoB:::Baz(size = 1)@size, 1))
        stopifnot(identical(evoA::FooWarn(size = 3.5)@size, 3.5))
      }
    }
  ),

  # Renaming a class -------------------------------------------------------

  scenario(
    name = "class-rename",
    expect = paste(
      "A renames Foo to Bar, keeping Foo's definition alive with",
      "deprecated_class(new = Bar). B uses Foo as an external and a direct",
      "parent, as a method signature, and as a saved instance.",
      "Constructing Foo warns, but everything else is silent, stale or",
      "rebuilt: subclasses, methods, and saved instances (even across an",
      "RDS round trip) keep Foo's identity and never become Bar. Foo's",
      "methods do not apply to Bar; a method registered on the Foo | Bar",
      "union handles both. Rebuilding B warns once at install time, when",
      "the saved instance is reconstructed through the deprecated",
      "constructor."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Bar := new_class(properties = list(size = class_double))
      Foo := deprecated_class(
        properties = list(size = class_double),
        new = Bar,
        when = "2.0.0"
      )
    },
    b = {
      Foo := new_external_class(package = "evoA")
      Baz := new_class(parent = Foo)
      BazDirect := new_class(parent = evoA::Foo)
      gen := new_generic("x")
      method(gen, Foo) <- function(x, ...) x@size
      saved <- evoA::Foo(size = 1)
    },
    b_test = {
      library(evoB)
      library(S7)
      child <- evoB:::Baz(size = 2)
      stopifnot(identical(evoB:::gen(child), 2))
      stopifnot(identical(evoB:::BazDirect(size = 3)@size, 3))
      path <- tempfile(fileext = ".rds")
      saveRDS(evoB:::saved, path)
      restored <- readRDS(path)
      unlink(path)
      stopifnot(
        S7_inherits(restored, evoA::Foo),
        identical(evoB:::gen(restored), 1)
      )
      if (packageVersion("evoA") >= "2.0.0") {
        # The deprecated constructor warns but still constructs a Foo
        warned <- FALSE
        x <- withCallingHandlers(
          evoA::Foo(size = 3),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(
          warned,
          S7_inherits(x, evoA::Foo),
          identical(evoB:::gen(x), 3)
        )
        # Nothing becomes a Bar, and Bar has no methods of its own
        stopifnot(
          !S7_inherits(x, evoA::Bar),
          !S7_inherits(child, evoA::Bar),
          !S7_inherits(restored, evoA::Bar)
        )
        err <- tryCatch(evoB:::gen(evoA::Bar(size = 4)), error = identity)
        stopifnot(
          inherits(err, "error"),
          grepl("Can't find method", conditionMessage(err), fixed = TRUE)
        )
        # A union signature covers both classes
        both := new_generic("x")
        method(both, evoA::Foo | evoA::Bar) <- function(x) x@size
        stopifnot(
          identical(both(child), 2),
          identical(both(evoA::Bar(size = 4)), 4)
        )
      }
    }
  ),

  # Removing a class -------------------------------------------------------

  scenario(
    name = "class-remove",
    expect = paste(
      "A retires Foo without replacement using deprecated_class(),",
      "keeping its custom constructor, lexical default, and validator.",
      "Class contexts stay silent, stale or rebuilt: external and direct",
      "subclasses (with invalid values still rejected), property types,",
      "unions, and method dispatch. Only an explicit constructor call",
      "warns. One boundary: a property default captured from a direct",
      "evoA::Foo reference still calls the constructor, so a stale B",
      "warns until rebuilt."
    ),
    a_v1 = {
      default_size <- 7
      Foo := new_class(
        properties = list(size = class_double),
        constructor = function(size = default_size) {
          new_object(S7_object(), size = size)
        },
        validator = function(self) {
          if (length(self@size) != 1L || self@size < 0) {
            "size must be non-negative and scalar"
          }
        }
      )
    },
    a_v2 = {
      default_size <- 7
      Foo := deprecated_class(
        properties = list(size = class_double),
        constructor = function(size = default_size) {
          new_object(S7_object(), size = size)
        },
        validator = function(self) {
          if (length(self@size) != 1L || self@size < 0) {
            "size must be non-negative and scalar"
          }
        },
        when = "2.0.0"
      )
    },
    b = {
      built_against <- as.character(getNamespaceVersion("evoA"))
      Foo := new_external_class(package = "evoA")
      Baz := new_class(parent = Foo)
      BazDirect := new_class(parent = evoA::Foo)
      Box := new_class(properties = list(item = Foo, optional = Foo | NULL))
      BoxDirect := new_class(properties = list(item = evoA::Foo))
      gen := new_generic("x")
      method(gen, Foo) <- function(x, ...) "B"
    },
    b_test = {
      library(evoB)
      library(S7)
      # Class contexts stay silent under either version
      withCallingHandlers(
        {
          stopifnot(
            identical(evoB:::Baz()@size, 7),
            identical(evoB:::BazDirect()@size, 7),
            S7_inherits(evoB:::Box()@item, evoA::Foo),
            S7_inherits(evoB:::Box()@optional, evoA::Foo),
            identical(evoB:::gen(evoB:::Baz()), "B")
          )
          for (constructor in list(evoB:::Baz, evoB:::BazDirect)) {
            err <- tryCatch(constructor(size = -1), error = identity)
            stopifnot(
              inherits(err, "error"),
              grepl(
                "size must be non-negative and scalar",
                conditionMessage(err),
                fixed = TRUE
              )
            )
          }
        },
        warning = function(w) stop(w)
      )
      if (packageVersion("evoA") >= "2.0.0") {
        # Explicit constructor calls warn
        warned <- FALSE
        x <- withCallingHandlers(
          evoA::Foo(size = 2),
          warning = function(w) {
            warned <<- TRUE
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(warned, identical(x@size, 2))
        # A stale direct property default still calls the constructor;
        # rebuilding generates a silent default
        box <- if (evoB:::built_against == "2.0.0") {
          withCallingHandlers(
            evoB:::BoxDirect(),
            warning = function(w) stop(w)
          )
        } else {
          warned <- FALSE
          value <- withCallingHandlers(
            evoB:::BoxDirect(),
            warning = function(w) {
              warned <<- TRUE
              invokeRestart("muffleWarning")
            }
          )
          stopifnot(warned)
          value
        }
        stopifnot(S7_inherits(box@item, evoA::Foo))
      }
    }
  ),

  # Moving a class ---------------------------------------------------------

  scenario(
    name = "class-move",
    expect = paste(
      "A moves Foo to evoACore, keeping evoA::Foo alive with",
      "deprecated_class(new = evoACore::Foo). Changing the package would",
      "change the class's identity, so the recommendation deliberately",
      "preserves evoA::Foo: B's subclass and methods keep working, stale",
      "or rebuilt, instances are not evoACore::Foo, and only constructor",
      "calls warn, naming the new home."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_core = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Foo := deprecated_class(
        properties = list(size = class_double),
        new = evoACore::Foo,
        when = "2.0.0"
      )
    },
    b = {
      Foo := new_external_class(package = "evoA")
      Baz := new_class(parent = Foo)
      gen := new_generic("x")
      method(gen, Foo) <- function(x, ...) x@size
    },
    b_test = {
      library(evoB)
      library(S7)
      x <- evoB:::Baz(size = 2)
      stopifnot(identical(x@size, 2), identical(evoB:::gen(x), 2))
      if (packageVersion("evoA") >= "2.0.0") {
        stopifnot(!S7_inherits(x, evoACore::Foo))
        message <- NULL
        value <- withCallingHandlers(
          evoA::Foo(size = 3),
          warning = function(w) {
            message <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        )
        stopifnot(
          grepl("evoACore::Foo()", message, fixed = TRUE),
          identical(evoB:::gen(value), 3)
        )
      } else {
        stopifnot(identical(evoB:::gen(evoA::Foo(size = 1)), 1))
      }
    }
  )
)

names(scenarios) <- vapply(scenarios, \(x) x$name, character(1))
