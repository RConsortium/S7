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
      "B's method lacks it. Installation, loading, and calls remain silent;",
      "R CMD check of B reports the mismatch in a NOTE."
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
      "must match exactly in development contexts. Ordinary installation",
      "succeeds, but calls and R CMD check fail against 2.0.0."
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
      "old default. Ordinary installation and loading are silent; R CMD",
      "check reports a NOTE. Calls receive the generic's new default."
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
      "A converts a single-dispatch generic to double dispatch. B fails to",
      "reinstall: multidispatch registration requires a list signature.",
      "The stale registration is a separate case checked below."
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
      "A renames a class but keeps the old name as an exported alias.",
      "External-class resolution follows the alias, so construction works",
      "for both stale and rebuilt B."
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

scenarios <- c(
  scenarios,
  list(
    scenario(
      name = "gen-rename-deprecated",
      expect = paste(
        "A renames a generic with deprecated_generic(). B imports the old",
        "name and registers a method. Registration stays silent; calls via",
        "the old name warn and both names reach B's method, stale or rebuilt."
      ),
      a_v1 = {
        gen1 := new_generic("x")
      },
      a_v2 = {
        gen2 := new_generic("x")
        gen1 := deprecated_generic(new = gen2, when = "2.0.0")
      },
      b = {
        BClass := new_class()
        method(gen1, BClass) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::BClass()
        stopifnot(identical(evoA::gen1(x), "B"))
        if ("gen2" %in% getNamespaceExports("evoA")) {
          stopifnot(identical(evoA::gen2(x), "B"))
          stopifnot(identical(
            S7::method(evoA::gen1, object = x),
            S7::method(evoA::gen2, object = x)
          ))
        }
      },
      b_ns = "importFrom(evoA, gen1)"
    ),
    scenario(
      name = "gen-rename-deprecated-external",
      expect = paste(
        "As gen-rename-deprecated, with deferred new_external_generic()",
        "registration. The old reference should resolve to the replacement."
      ),
      a_v1 = {
        gen1 := new_generic("x")
      },
      a_v2 = {
        gen2 := new_generic("x")
        gen1 := deprecated_generic(new = gen2, when = "2.0.0")
      },
      b = {
        gen1 := new_external_generic(package = "evoA", dispatch_args = "x")
        BClass := new_class()
        method(gen1, BClass) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::BClass()
        stopifnot(identical(evoA::gen1(x), "B"))
        if ("gen2" %in% getNamespaceExports("evoA")) {
          stopifnot(identical(evoA::gen2(x), "B"))
        }
      }
    ),
    scenario(
      name = "gen-retire-deprecated",
      expect = paste(
        "A retires a generic without replacing it. B's imported registration",
        "should survive both an upstream-only upgrade and rebuilding B;",
        "calling the generic warns and still dispatches."
      ),
      a_v1 = {
        gen := new_generic("x")
      },
      a_v2 = {
        gen := new_generic("x")
        gen := deprecated_generic(old = gen, when = "2.0.0")
      },
      b = {
        BClass := new_class()
        method(gen, BClass) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        stopifnot(identical(evoA::gen(evoB:::BClass()), "B"))
      },
      b_ns = "importFrom(evoA, gen)"
    ),
    scenario(
      name = "gen-move-package-deprecated",
      expect = paste(
        "A moves a generic to evoACore and exports deprecated_generic(new =",
        "evoACore::gen). B's methods should be callable through both packages.",
        "This exercises serialization of a wrapper around a foreign generic."
      ),
      a_v1 = {
        gen := new_generic("x")
      },
      a_core = {
        gen := new_generic("x")
      },
      a_v2 = {
        gen := deprecated_generic(new = evoACore::gen, when = "2.0.0")
      },
      b = {
        BClass := new_class()
        method(gen, BClass) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::BClass()
        if (requireNamespace("evoACore", quietly = TRUE)) {
          stopifnot(identical(evoACore::gen(x), "B"))
        }
        stopifnot(identical(evoA::gen(x), "B"))
      },
      b_ns = "importFrom(evoA, gen)"
    ),
    scenario(
      name = "class-rename-deprecated-external",
      expect = paste(
        "A renames Foo to Bar with deprecated_class(). B uses an external",
        "parent and registers a method on the external class. Check both",
        "construction and dispatch, including an already-installed subclass."
      ),
      a_v1 = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_v2 = {
        Bar := new_class(properties = list(size = class_double))
        Foo := deprecated_class(new = Bar, when = "2.0.0")
      },
      b = {
        Foo := new_external_class(package = "evoA")
        Baz := new_class(parent = Foo, properties = list(extra = class_double))
        gen := new_generic("x")
        method(gen, Foo) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::Baz(size = 1, extra = 2)
        stopifnot(identical(x@size, 1), identical(x@extra, 2))
        stopifnot(identical(evoB:::gen(evoA::Foo(size = 1)), "B"))
        stopifnot(identical(evoB:::gen(x), "B"))
      }
    ),
    scenario(
      name = "class-rename-deprecated-direct",
      expect = paste(
        "A renames Foo to Bar with deprecated_class(). B stores a direct",
        "parent and method signature. A rebuilt B should use Bar; a stale",
        "B retains the old class identity and may need rebuilding."
      ),
      a_v1 = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_v2 = {
        Bar := new_class(properties = list(size = class_double))
        Foo := deprecated_class(new = Bar, when = "2.0.0")
      },
      b = {
        Baz := new_class(parent = evoA::Foo)
        gen := new_generic("x")
        method(gen, evoA::Foo) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::Baz(size = 1)
        stopifnot(identical(evoB:::gen(x), "B"))
        stopifnot(identical(evoB:::gen(evoA::Foo(size = 1)), "B"))
      }
    ),
    scenario(
      name = "class-retire-deprecated",
      expect = paste(
        "A retires Foo without a replacement. B uses it as an external parent,",
        "property type, union, and method signature. Class contexts stay silent;",
        "only an explicit call to the deprecated constructor warns."
      ),
      a_v1 = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_v2 = {
        Foo := new_class(properties = list(size = class_double))
        Foo := deprecated_class(old = Foo, when = "2.0.0")
      },
      b = {
        Foo := new_external_class(package = "evoA")
        Baz := new_class(parent = Foo)
        Box := new_class(properties = list(item = Foo, optional = Foo | NULL))
        gen := new_generic("x")
        method(gen, Foo) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::Baz(size = 1)
        box <- evoB:::Box(item = x, optional = NULL)
        stopifnot(S7::S7_inherits(box@item, evoA::Foo))
        stopifnot(identical(evoB:::gen(x), "B"))
        stopifnot(identical(evoA::Foo(size = 2)@size, 2))
      }
    ),
    scenario(
      name = "class-move-package-deprecated",
      expect = paste(
        "A moves Foo to evoACore and keeps a deprecated alias. Check external",
        "parent construction and method dispatch for stale and rebuilt B."
      ),
      a_v1 = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_core = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_v2 = {
        Foo := deprecated_class(new = evoACore::Foo, when = "2.0.0")
      },
      b = {
        Foo := new_external_class(package = "evoA")
        Baz := new_class(parent = Foo)
        gen := new_generic("x")
        method(gen, Foo) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::Baz(size = 1)
        stopifnot(identical(x@size, 1))
        stopifnot(identical(evoB:::gen(evoA::Foo(size = 1)), "B"))
        stopifnot(identical(evoB:::gen(x), "B"))
      }
    ),
    scenario(
      name = "class-rename-serialized-instance",
      expect = paste(
        "B saves an instance of A's Foo at installation. A renames Foo to Bar",
        "with a deprecated alias. The saved instance keeps its old identity:",
        "a stale B cannot treat it as the replacement class."
      ),
      a_v1 = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_v2 = {
        Bar := new_class(properties = list(size = class_double))
        Foo := deprecated_class(new = Bar, when = "2.0.0")
      },
      b = {
        saved <- evoA::Foo(size = 1)
      },
      b_test = {
        library(evoB)
        stopifnot(S7::S7_inherits(evoB:::saved, evoA::Foo))
      }
    ),
    scenario(
      name = "class-rename-prop-deprecated",
      expect = paste(
        "A renames count to size, preserving its default. The old constructor",
        "argument, reads, and changed writes warn and delegate after rebuilding",
        "B. A stale subclass retains the old stored property and fails",
        "validation. Rebuilt print() and str() stay silent."
      ),
      a_v1 = {
        Foo := new_class(
          properties = list(count = new_property(class_double, default = 7))
        )
      },
      a_v2 = {
        Foo := new_class(
          properties = list(
            size = new_property(class_double, default = 7),
            deprecated_property("count", new = "size", when = "2.0.0")
          )
        )
      },
      b = {
        Foo := new_external_class(package = "evoA")
        Baz := new_class(parent = Foo)
      },
      b_test = {
        library(evoB)
        stopifnot(identical(evoA::Foo()@count, 7))
        x <- evoB:::Baz(count = 2)
        stopifnot(identical(x@count, 2))
        x@count <- 3
        stopifnot(identical(x@count, 3))
        withCallingHandlers(
          {
            print(x)
            str(x)
          },
          warning = function(w) stop(w)
        )
        if ("size" %in% S7::prop_names(x)) {
          stopifnot(identical(x@size, 3))
        }
      }
    ),
    scenario(
      name = "class-retire-prop-deprecated",
      expect = paste(
        "A retires count without replacing it, preserving its type and default.",
        "Construction remains silent; reads and changed writes warn. Printing",
        "omits the deprecated property."
      ),
      a_v1 = {
        Foo := new_class(
          properties = list(count = new_property(class_double, default = 7))
        )
      },
      a_v2 = {
        Foo := new_class(
          properties = list(
            deprecated_property(
              "count",
              when = "2.0.0",
              class = class_double,
              default = 7
            )
          )
        )
      },
      b = {},
      b_test = {
        library(evoB)
        x <- withCallingHandlers(evoA::Foo(count = 2), warning = function(w) {
          stop(w)
        })
        stopifnot(identical(evoA::Foo()@count, 7), identical(x@count, 2))
        x@count <- 3
        stopifnot(identical(x@count, 3))
        withCallingHandlers(
          {
            print(x)
            str(x)
          },
          warning = function(w) stop(w)
        )
      }
    ),
    scenario(
      name = "class-deprecated-prop-same-value",
      expect = paste(
        "A deprecates count in favor of size. Equal constructor arguments and",
        "an explicit write of the current value remain silent, including in",
        "stop mode. This is the documented initialization exception."
      ),
      a_v1 = {
        Foo := new_class(properties = list(count = class_double))
      },
      a_v2 = {
        Foo := new_class(
          properties = list(
            size = class_double,
            deprecated_property("count", new = "size", when = "2.0.0")
          )
        )
      },
      b = {},
      b_test = {
        library(evoB)
        warnings <- 0L
        withCallingHandlers(
          {
            x <- if (packageVersion("evoA") >= "2.0.0") {
              evoA::Foo(size = 2, count = 2)
            } else {
              evoA::Foo(count = 2)
            }
            x@count <- 2
          },
          warning = function(w) warnings <<- warnings + 1L
        )
        stopifnot(warnings == 0L)
      }
    ),
    scenario(
      name = "gen-export-rename-deprecated",
      expect = paste(
        "A exports the original gen1 generic as gen2 and deprecates gen1 with",
        "new_label. Both exports share B's methods, stale or rebuilt, and",
        "the warning recommends gen2 even though the generic's name is gen1."
      ),
      a_v1 = {
        gen1 := new_generic("x")
      },
      a_v2 = {
        gen1 := new_generic("x")
        gen2 <- gen1
        gen1 := deprecated_generic(
          new = gen2,
          when = "2.0.0",
          new_label = "gen2()"
        )
      },
      b = {
        BClass := new_class()
        method(gen1, BClass) <- function(x, ...) "B"
      },
      b_test = {
        library(evoB)
        x <- evoB:::BClass()
        warnings <- character()
        value <- withCallingHandlers(evoA::gen1(x), warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
        })
        stopifnot(identical(value, "B"))
        if (packageVersion("evoA") >= "2.0.0") {
          stopifnot(
            identical(evoA::gen2(x), "B"),
            length(warnings) == 1L,
            grepl("Please use `gen2()` instead.", warnings, fixed = TRUE)
          )
        }
      },
      b_ns = "importFrom(evoA, gen1)"
    ),
    scenario(
      name = "class-export-rename-deprecated",
      expect = paste(
        "A exports the original Foo class as Bar and deprecates Foo with",
        "new_label. The class identity stays Foo, so B's installed subclass,",
        "methods, and saved instance continue to work without rebuilding."
      ),
      a_v1 = {
        Foo := new_class(properties = list(size = class_double))
      },
      a_v2 = {
        Foo := new_class(properties = list(size = class_double))
        Bar <- Foo
        Foo := deprecated_class(
          new = Bar,
          when = "2.0.0",
          new_label = "Bar()"
        )
      },
      b = {
        Foo := new_external_class(package = "evoA")
        Baz := new_class(parent = Foo)
        gen := new_generic("x")
        method(gen, Foo) <- function(x, ...) x@size
        saved <- evoA::Foo(size = 1)
      },
      b_test = {
        library(evoB)
        child <- evoB:::Baz(size = 2)
        stopifnot(
          identical(evoB:::gen(child), 2),
          identical(evoB:::gen(evoB:::saved), 1)
        )
        warnings <- character()
        x <- withCallingHandlers(evoA::Foo(size = 3), warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
        })
        stopifnot(identical(evoB:::gen(x), 3))
        if (packageVersion("evoA") >= "2.0.0") {
          stopifnot(
            S7::S7_inherits(child, evoA::Bar),
            S7::S7_inherits(evoB:::saved, evoA::Bar),
            length(warnings) == 1L,
            grepl("Please use `Bar()` instead.", warnings, fixed = TRUE)
          )
        }
      }
    ),
    scenario(
      name = "class-retire-prop-validator",
      expect = paste(
        "A retires count while preserving its class, default, and validator.",
        "Negative constructor and assignment values remain invalid, and a",
        "rejected assignment leaves the previous value intact."
      ),
      a_v1 = {
        Foo := new_class(
          properties = list(
            count = new_property(
              class_double,
              default = 1,
              validator = function(value) {
                if (any(value < 0)) "must be non-negative"
              }
            )
          )
        )
      },
      a_v2 = {
        Foo := new_class(
          properties = list(
            deprecated_property(
              "count",
              when = "2.0.0",
              class = class_double,
              default = 1,
              validator = function(value) {
                if (any(value < 0)) "must be non-negative"
              }
            )
          )
        )
      },
      b = {},
      b_test = {
        library(evoB)
        x <- evoA::Foo()
        stopifnot(identical(x@count, 1))
        invalid_constructor <- tryCatch(evoA::Foo(count = -1), error = identity)
        invalid_assignment <- tryCatch(x@count <- -1, error = identity)
        stopifnot(
          inherits(invalid_constructor, "error"),
          grepl("must be non-negative", conditionMessage(invalid_constructor)),
          inherits(invalid_assignment, "error"),
          grepl("must be non-negative", conditionMessage(invalid_assignment)),
          identical(x@count, 1)
        )
        x@count <- 2
        stopifnot(identical(x@count, 2))
      }
    )
  )
)

names(scenarios) <- vapply(scenarios, \(x) x$name, character(1))

# Exercise the same cross-package transitions with each signaling policy.
for (method in c("lifecycle(warn)", "lifecycle(stop)")) {
  for (name in c(
    "gen-rename-deprecated",
    "class-retire-deprecated",
    "class-rename-prop-deprecated",
    "class-retire-prop-deprecated",
    "class-deprecated-prop-same-value"
  )) {
    sc <- scenarios[[name]]
    sc$name <- paste0(
      name,
      "-",
      if (method == "lifecycle(warn)") "warn" else "stop"
    )
    sc$a_v2 <- sub(
      'when = "2.0.0"',
      paste0('when = "2.0.0", method = "', method, '"'),
      sc$a_v2,
      fixed = TRUE
    )
    sc$a_imports <- "lifecycle"
    sc$expect <- paste(
      sc$expect,
      "Signaling policy:",
      method,
      "(stop mode signals errors instead of warnings)."
    )
    scenarios[[sc$name]] <- sc
  }
}
