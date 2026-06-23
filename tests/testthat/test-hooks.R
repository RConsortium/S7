test_that("S7_on_load() doesn't accumulate hooks across repeated loads", {
  upstream := local_package({
    gen := new_generic("x")
  })
  expect_length(package_hooks("upstream"), 0)

  downstream := local_package({
    Foo := new_class()
    gen := new_external_generic("upstream", dispatch_args = "x")
    method(gen, Foo) <- \(x) "dispatched"
  })
  S7_on_load_(downstream)
  expect_length(package_hooks("upstream"), 1)
  S7_on_load_(downstream)
  expect_length(package_hooks("upstream"), 1)
})

test_that("S7_on_load() waits until all external union arms are available", {
  generic_pkg <- local_package("external_union_generic", {
    gen := new_generic("x")
  })
  upstream_a <- local_package("external_union_a", {
    A := new_class()
  })
  downstream <- local_package("external_union_downstream", {
    .onLoad <- function(...) S7_on_load()
    gen <- new_external_generic(
      package = "external_union_generic",
      name = "gen",
      dispatch_args = "x"
    )
    A := new_external_class(package = "external_union_a")
    B := new_external_class(package = "external_union_b")
    method(gen, A | B) <- function(x) "union"
  })

  downstream$.onLoad()
  expect_error(
    generic_pkg$gen(upstream_a$A()),
    class = "S7_error_method_not_found"
  )

  upstream_b <- local_package("external_union_b", {
    B := new_class()
  })
  downstream$.onLoad()
  expect_equal(generic_pkg$gen(upstream_a$A()), "union")
  expect_equal(generic_pkg$gen(upstream_b$B()), "union")
})

test_that("S7_on_load() and S7_on_unload() handle external classes", {
  upstream <- local_package("external_class_unload", {
    Foo := new_class()
  })
  downstream <- local_package("downstream_external_class_unload", {
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    own_generic := new_generic("x")
    Foo := new_external_class(package = "external_class_unload")
    method(own_generic, Foo) <- function(x) "from external class"
  })
  downstream$.onLoad()
  expect_equal(downstream$own_generic(upstream$Foo()), "from external class")

  downstream$.onUnload()
  expect_error(
    downstream$own_generic(upstream$Foo()),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() handles external classes unloaded first", {
  generic_pkg <- local_package("external_class_unload_first_generic", {
    gen := new_generic(dispatch_args = "x")
  })
  downstream <- local_package("external_class_unload_first_downstream", {
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    gen := new_external_generic(
      package = "external_class_unload_first_generic",
      dispatch_args = "x"
    )
    Foo := new_external_class(
      package = "external_class_unload_first_dep"
    )
    method(gen, Foo) <- \(x) "from external class"
  })
  obj <- local({
    class_pkg <- local_package("external_class_unload_first_dep", {
      Foo := new_class()
    })
    downstream$.onLoad()

    obj <- class_pkg$Foo()
    expect_equal(generic_pkg$gen(obj), "from external class")
    obj
  })

  expect_identical(isNamespaceLoaded("external_class_unload_first_dep"), FALSE)
  downstream$.onUnload()
  expect_error(
    generic_pkg$gen(obj),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() unregisters methods and removes hooks", {
  upstream := local_package({
    gen := new_generic("x")
  })
  downstream := local_package({
    Foo := new_class()
    gen := new_external_generic("upstream", dispatch_args = "x")
    method(gen, Foo) <- \(x) "dispatched"
  })
  S7_on_load_(downstream)

  S7_on_unload_(downstream)
  expect_length(package_hooks("upstream"), 0)
  expect_error(
    upstream$gen(downstream$Foo()),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() unregisters base operator methods", {
  local_methods(base_ops[["+"]])

  downstream <- local_package("downstream_base_ops_unload", {
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    Foo := new_class()
    method(`+`, list(Foo, Foo)) <- function(e1, e2) "dispatched"
  })
  downstream$.onLoad()
  expect_equal(downstream$Foo() + downstream$Foo(), "dispatched")

  downstream$.onUnload()
  expect_error(
    downstream$Foo() + downstream$Foo(),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() doesn't remove methods registered by another package", {
  upstream <- local_package("upstream_conflict", {
    gen := new_generic("x")
  })
  first <- local_package("downstream_first", {
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    gen := new_external_generic("upstream_conflict", dispatch_args = "x")
    method(gen, class_character) <- function(x) "first"
  })
  first$.onLoad()
  expect_equal(upstream$gen("x"), "first")

  second <- NULL
  expect_message(
    second <- local_package("downstream_second", {
      .onLoad <- function(...) S7_on_load()
      .onUnload <- function(...) S7_on_unload()
      gen := new_external_generic("upstream_conflict", dispatch_args = "x")
      method(gen, class_character) <- function(x) "second"
    }),
    "Overwriting method"
  )
  second$.onLoad()
  expect_equal(upstream$gen("x"), "second")

  first$.onUnload()
  expect_equal(upstream$gen("x"), "second")
})

test_that("S7_on_load() removes hooks for deleted external methods", {
  upstream <- local_package("upstream_deleted", {
    gen := new_generic("x")
  })
  downstream <- local_package("downstream_deleted", {
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    Foo := new_class()
    gen := new_external_generic("upstream_deleted", dispatch_args = "x")
    method(gen, Foo) <- function(x) "dispatched"
  })
  downstream$.onLoad()
  expect_length(package_hooks("upstream_deleted"), 1)

  eval(quote(method(gen, Foo) <- NULL), downstream)
  expect_error(
    upstream$gen(downstream$Foo()),
    class = "S7_error_method_not_found"
  )

  downstream$.onLoad()
  expect_length(package_hooks("upstream_deleted"), 0)
  expect_error(
    upstream$gen(downstream$Foo()),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() honors external generic version gates", {
  downstream <- local_package("downstream_version_gate_unload", {
    .onLoad <- function(...) S7_on_load()
    .onUnload <- function(...) S7_on_unload()
    gen := new_external_generic(
      "upstream_version_gate_unload",
      dispatch_args = "x",
      version = "1.0.0"
    )
    method(gen, class_character) <- function(x) "downstream"
  })
  upstream <- local_package("upstream_version_gate_unload", {
    gen <- function(x) "not an S7 generic"
  })

  downstream$.onLoad()
  expect_equal(upstream$gen("x"), "not an S7 generic")
  expect_no_error(downstream$.onUnload())
})

test_that("S7_on_unload() unregisters methods when a real package is unloaded (#316)", {
  skip_if(getRversion() < "4.1" && Sys.info()[["sysname"]] == "Windows")
  skip_if(quick_test())

  tmp_lib <- local_libpath()
  local_install_and_attach(test_path("t0"), tmp_lib)
  local_install_and_attach(test_path("t2"), tmp_lib)

  expect_equal(t0::an_s7_generic("x"), "foo")

  # t2's .onUnload() calls S7_on_unload(), which unregisters the methods
  # it registered for t0's generics and removes its hooks for t1
  unloadNamespace("t2")
  expect_null(t0::an_s7_generic@methods[["character"]])
  expect_length(package_hooks("t1"), 0)
})

test_that("S7_on_build() removes only generic sentinels from the namespace", {
  ns <- new.env(parent = emptyenv())
  ns$keep_fun <- function() {}
  gen := new_external_generic("pkg", dispatch_args = "x")
  ns$keep_ext <- gen
  ns$drop_me <- generic_sentinel(gen)

  strip_generic_sentinels(ns)
  expect_setequal(names(ns), c("keep_ext", "keep_fun"))
})

test_that("S7_on_build() is a no-op when there are no sentinels", {
  ns <- new.env(parent = emptyenv())
  ns$x <- 1
  ns$f <- function() {}
  strip_generic_sentinels(ns)
  expect_setequal(names(ns), c("f", "x"))
})

test_that("S7_on_build() strips foreign S3 and S4 generics from a real package (#364)", {
  skip_if(getRversion() < "4.1" && Sys.info()[["sysname"]] == "Windows")
  skip_if(quick_test())
  # if this fails interactively, ensure you have dev S7 installed

  tmp_lib <- local_libpath()

  # t3 defines an S3 generic and an S4 generic; t4 registers S7 methods for
  # both (plus the base generic `$`), so building t4 would otherwise embed
  # copies of those generics.
  local_install_and_attach(test_path("t3"), tmp_lib)
  local_install_and_attach(test_path("t4"), tmp_lib)

  obj <- t4::t4_class()
  expect_equal(t3_s3(obj), "s3-dispatch")
  expect_equal(t3_s4(obj), "s4-dispatch")

  # S7_on_build() removed the generics that `method<-` would otherwise have
  # embedded in t4's namespace.
  ns <- asNamespace("t4")
  expect_false(exists("t3_s3", envir = ns, inherits = FALSE))
  expect_false(exists("t3_s4", envir = ns, inherits = FALSE))
  expect_false(exists("$", envir = ns, inherits = FALSE))
})
