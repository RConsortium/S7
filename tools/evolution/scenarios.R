# Scenario definitions for the evolution compat lab. See run.R and README.md.
#
# Each scenario describes an upstream package evoA at version 1.0.0 (`a_v1`)
# and 2.0.0 (`a_v2`), a downstream package evoB (`b`) written against 1.0.0,
# and a smoke test (`b_test`) that must pass against 1.0.0. What we learn is
# how (and *when*) each scenario fails against 2.0.0.
#
# Code is given unquoted and deparsed into R files in the fixture packages,
# so it follows user-facing S7 style, not S7-internal style.

# `b_ns` gives extra NAMESPACE directives for evoB: registering a method on
# another package's generic requires importing the generic, since
# `method(evoA::gen, ...) <- f` is a replacement call and can't assign to
# `evoA::gen`.
#
# `a_core` gives code for a third package, evoACore, that exists only at
# version 2.0.0 (it's installed just before evoA 2.0.0, which imports it).
# Use it to model evoA moving a generic or class to a lower-level package.
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

scenarios <- list(
  # Generics ------------------------------------------------------------

  scenario(
    name = "gen-add-arg",
    expect = paste(
      "A adds an optional argument to a generic that has `...`;",
      "B's method lacks it. Expect warnings when evoB is (re)installed and",
      "loaded against 2.0.0; calls still work."
    ),
    a_v1 = {
      gen := new_generic("x")
    },
    a_v2 = {
      gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
        S7_dispatch()
      })
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-add-arg-fixed",
    expect = paste(
      "As gen-add-arg, but B's method already has the argument A is about to",
      "add (the recommended transition). Expect everything OK against both",
      "versions, since methods may have arguments the generic lacks."
    ),
    a_v1 = {
      gen := new_generic("x")
    },
    a_v2 = {
      gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
        S7_dispatch()
      })
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, verbose = FALSE, ...) {
        paste0("B:", x@val, if (verbose) "!")
      }
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-add-arg-nodots",
    expect = paste(
      "A adds an argument to a generic *without* `...`, where method formals",
      "must match exactly. Expect evoB to fail to install against 2.0.0, with",
      "no version of B that works for both."
    ),
    a_v1 = {
      gen := new_generic("x", fun = function(x) S7_dispatch())
    },
    a_v2 = {
      gen := new_generic("x", fun = function(x, verbose = FALSE) S7_dispatch())
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-remove-arg",
    expect = paste(
      "A removes an optional argument; B's method still has it. Expect this",
      "to be silent (methods may have extra arguments) so B can drop the",
      "argument at leisure."
    ),
    a_v1 = {
      gen := new_generic("x", fun = function(x, verbose = FALSE, ...) {
        S7_dispatch()
      })
    },
    a_v2 = {
      gen := new_generic("x")
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, verbose = FALSE, ...) {
        paste0("B:", x@val)
      }
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-change-default",
    expect = paste(
      "A changes the default of a non-dispatch argument; B's method uses the",
      "old default. Expect a default-mismatch warning at install/load against",
      "2.0.0 (and B matching 2.0.0 would warn against 1.0.0)."
    ),
    a_v1 = {
      gen := new_generic("x", fun = function(x, drop = FALSE, ...) {
        S7_dispatch()
      })
    },
    a_v2 = {
      gen := new_generic("x", fun = function(x, drop = TRUE, ...) S7_dispatch())
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
      stopifnot(startsWith(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-rename",
    expect = paste(
      "A renames a generic with no alias. Expect evoB to fail to install",
      "against 2.0.0 (`evoA::gen1` no longer exists)."
    ),
    a_v1 = {
      gen1 := new_generic("x")
    },
    a_v2 = {
      gen2 := new_generic("x")
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen1, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen1(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen1)"
  ),

  scenario(
    name = "gen-rename-alias",
    expect = paste(
      "A renames a generic but keeps the old name as an exported alias.",
      "Expect B (which still registers on the old name) to keep working, and",
      "methods to be reachable through both names."
    ),
    a_v1 = {
      gen1 := new_generic("x")
    },
    a_v2 = {
      gen2 := new_generic("x")
      gen1 <- gen2
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen1, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen1(x), "B:1"))
      if ("gen2" %in% getNamespaceExports("evoA")) {
        stopifnot(identical(evoA::gen2(x), "B:1"))
      }
    },
    b_ns = "importFrom(evoA, gen1)"
  ),

  scenario(
    name = "gen-rename-wrapper",
    expect = paste(
      "A renames a generic and turns the old name into a deprecating wrapper",
      "function. Callers of the old name keep working (with a warning), but",
      "does downstream method registration on the old name still work, given",
      "that the wrapper is not a generic?"
    ),
    a_v1 = {
      gen1 := new_generic("x")
    },
    a_v2 = {
      gen2 := new_generic("x")
      gen1 <- function(x, ...) {
        .Deprecated("gen2")
        gen2(x, ...)
      }
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen1, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(suppressWarnings(evoA::gen1(x)), "B:1"))
    },
    b_ns = "importFrom(evoA, gen1)"
  ),

  scenario(
    name = "gen-add-dispatch-arg",
    expect = paste(
      "A converts a single-dispatch generic to double dispatch. Expect evoB",
      "to fail to install against 2.0.0: the method signature and formals no",
      "longer conform."
    ),
    a_v1 = {
      gen := new_generic("x")
    },
    a_v2 = {
      gen := new_generic(c("x", "y"))
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-move-package",
    expect = paste(
      "A moves a generic to a lower-level package (evoACore) and re-exports",
      "it. Expect B (which imports the generic from evoA) to keep working:",
      "registration follows the generic object to its home package."
    ),
    a_v1 = {
      gen := new_generic("x")
    },
    a_core = {
      gen := new_generic("x")
    },
    a_v2 = {},
    a_ns = c("importFrom(evoACore, gen)", "export(gen)"),
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
      if (requireNamespace("evoACore", quietly = TRUE)) {
        stopifnot(identical(evoACore::gen(x), "B:1"))
      }
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  scenario(
    name = "gen-move-package-copy",
    expect = paste(
      "As gen-move-package, but evoA re-exports with a binding copy",
      "(`gen <- evoACore::gen`) instead of a NAMESPACE re-export. Expect",
      "dispatch to fail: the copy serialized into evoA has its own methods",
      "table, separate from the one B registers into."
    ),
    a_v1 = {
      gen := new_generic("x")
    },
    a_core = {
      gen := new_generic("x")
    },
    a_v2 = {
      gen <- evoACore::gen
    },
    b = {
      BClass := new_class(properties = list(val = class_double))
      method(gen, BClass) <- function(x, ...) paste0("B:", x@val)
    },
    b_test = {
      library(evoB)
      x <- evoB:::BClass(val = 1)
      stopifnot(identical(evoA::gen(x), "B:1"))
    },
    b_ns = "importFrom(evoA, gen)"
  ),

  # Classes -------------------------------------------------------------

  scenario(
    name = "class-add-prop",
    expect = paste(
      "A adds a property that doesn't clash with B's subclass. Expect",
      "everything OK once evoB is rebuilt; the stale stage shows what",
      "happens when only evoA is upgraded."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Foo := new_class(
        properties = list(
          size = class_double,
          extra = class_character
        )
      )
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(obj@size, 1), identical(obj@y, 2))
    }
  ),

  scenario(
    name = "class-add-prop-clash",
    expect = paste(
      "A adds a property whose name B's subclass already uses, with an",
      "incompatible type. Expect evoB to fail to (re)install against 2.0.0",
      "because the override doesn't extend the parent property."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Foo := new_class(
        properties = list(
          size = class_double,
          y = class_character
        )
      )
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(obj@y, 2))
    }
  ),

  scenario(
    name = "class-remove-prop",
    expect = paste(
      "A removes a property outright. Expect evoB to still install, but its",
      "uses of the property (constructor argument, `@count`) fail at run",
      "time, i.e. in tests/examples."
    ),
    a_v1 = {
      Foo := new_class(
        properties = list(
          size = class_double,
          count = class_double
        )
      )
    },
    a_v2 = {
      Foo := new_class(properties = list(size = class_double))
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoA::Foo(size = 1, count = 2)
      stopifnot(identical(obj@count, 2))
    }
  ),

  scenario(
    name = "class-remove-prop-deprecated",
    expect = paste(
      "A deprecates a property by replacing it with a dynamic property whose",
      "getter warns. Expect B's reads of `@count` to keep working against",
      "2.0.0, now with a deprecation warning."
    ),
    a_v1 = {
      Foo := new_class(
        properties = list(
          size = class_double,
          count = class_double
        )
      )
    },
    a_v2 = {
      Foo := new_class(
        properties = list(
          size = class_double,
          count = new_property(
            class_double,
            getter = function(self) {
              warning("@count is deprecated; use @size instead")
              self@size
            }
          )
        )
      )
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoA::Foo(size = 1)
      invisible(obj@count)
    }
  ),

  scenario(
    name = "class-narrow-prop",
    expect = paste(
      "A narrows a property's type from numeric to integer. Expect evoB to",
      "install fine but fail at run time when it constructs an instance with",
      "a double."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_numeric))
    },
    a_v2 = {
      Foo := new_class(properties = list(size = class_integer))
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoA::Foo(size = 1)
      stopifnot(identical(obj@size, 1))
    }
  ),

  scenario(
    name = "class-rename",
    expect = paste(
      "A renames a class with no alias. Expect evoB to fail to (re)install",
      "against 2.0.0: the external class <evoA::Foo> can't be resolved, for",
      "either the subclass or the method registration."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Bar := new_class(properties = list(size = class_double))
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
      bgen := new_generic("x")
      method(bgen, AFoo) <- function(x, ...) "hit"
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(evoB:::bgen(obj), "hit"))
    }
  ),

  scenario(
    name = "class-rename-alias-external",
    expect = paste(
      "A renames a class but keeps the old name as an exported alias. Expect",
      "this NOT to help B when B refers to the class with",
      "new_external_class(): resolution checks the class's real name."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Bar := new_class(properties = list(size = class_double))
      Foo <- Bar
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(obj@size, 1))
    }
  ),

  scenario(
    name = "class-rename-alias-direct",
    expect = paste(
      "As class-rename-alias-external, but B uses the class object directly",
      "(`parent = evoA::Foo`). Expect a rebuilt B to work, since the alias",
      "binding points to a real class (now named evoA::Bar)."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Bar := new_class(properties = list(size = class_double))
      Foo <- Bar
    },
    b = {
      Baz := new_class(parent = evoA::Foo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(obj@size, 1))
    }
  ),

  scenario(
    name = "class-make-abstract",
    expect = paste(
      "A makes a class abstract. Expect B's subclass to be unaffected (the",
      "smoke test only builds the subclass); direct `evoA::Foo()` calls would",
      "fail at run time."
    ),
    a_v1 = {
      Foo := new_class(properties = list(size = class_double))
    },
    a_v2 = {
      Foo := new_class(properties = list(size = class_double), abstract = TRUE)
    },
    b = {
      AFoo <- new_external_class("evoA", "Foo")
      Baz := new_class(parent = AFoo, properties = list(y = class_double))
    },
    b_test = {
      library(evoB)
      obj <- evoB:::Baz(size = 1, y = 2)
      stopifnot(identical(obj@size, 1))
    }
  )
)

names(scenarios) <- vapply(scenarios, function(x) x$name, character(1))
