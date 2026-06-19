test_that("S7_on_load() doesn't accumulate hooks across repeated loads", {
  upstream <- local_package("upstream", gen := new_generic("x"))
  expect_length(package_hooks("upstream"), 0)

  downstream <- local_package(
    "downstream",
    Foo := new_class(),
    gen := new_external_generic("upstream", dispatch_args = "x"),
    method(gen, Foo) <- \(x) "dispatched"
  )
  S7_on_load_(downstream)
  expect_length(package_hooks("upstream"), 1)
  S7_on_load_(downstream)
  expect_length(package_hooks("upstream"), 1)
})

test_that("S7_on_load() doesn't duplicate hooks when registrars error", {
  upstream_generic <- local_package(
    "hookgenericpkg",
    gen := new_generic("x")
  )
  upstream_class <- local_package(
    "hookclasspkg",
    Real := new_class()
  )
  downstream <- local_package(
    "downstreamerrorhook",
    .onLoad <- function(...) S7_on_load(),
    gen <- new_external_generic(
      package = "hookgenericpkg",
      name = "gen",
      dispatch_args = "x"
    ),
    Missing <- new_external_class(
      package = "hookclasspkg",
      name = "Missing"
    ),
    method(gen, Missing) <- function(x) "dispatched"
  )

  expect_snapshot(downstream$.onLoad(), error = TRUE)
  expect_length(package_hooks("hookgenericpkg"), 1)
  expect_length(package_hooks("hookclasspkg"), 1)

  expect_snapshot(downstream$.onLoad(), error = TRUE)
  expect_length(package_hooks("hookgenericpkg"), 1)
  expect_length(package_hooks("hookclasspkg"), 1)

  evalq(method(gen, Missing) <- NULL, downstream)
  expect_length(package_hooks("hookgenericpkg"), 0)
  expect_length(package_hooks("hookclasspkg"), 0)

  invisible(upstream_generic)
  invisible(upstream_class)
})

test_that("S7_on_load() removes stale hooks when hook records are lost", {
  upstream_generic <- local_package(
    "hooklostgenericpkg",
    gen := new_generic("x")
  )
  upstream_class <- local_package(
    "hooklostclasspkg",
    Real := new_class()
  )
  downstream <- local_package(
    "hooklostdownstream",
    .onLoad <- function(...) S7_on_load(),
    gen <- new_external_generic(
      package = "hooklostgenericpkg",
      name = "gen",
      dispatch_args = "x"
    ),
    Missing <- new_external_class(
      package = "hooklostclasspkg",
      name = "Missing"
    ),
    method(gen, Missing) <- function(x) "dispatched"
  )

  expect_snapshot(downstream$.onLoad(), error = TRUE)
  expect_length(package_hooks("hooklostgenericpkg"), 1)
  expect_length(package_hooks("hooklostclasspkg"), 1)

  `hooks_packages<-`("hooklostdownstream", character())

  expect_snapshot(downstream$.onLoad(), error = TRUE)
  expect_length(package_hooks("hooklostgenericpkg"), 1)
  expect_length(package_hooks("hooklostclasspkg"), 1)

  invisible(upstream_generic)
  invisible(upstream_class)
})

test_that("S7_on_load() registers methods dispatching on an external class", {
  upstream := local_package(
    Foo := new_class()
  )
  downstream := local_package(
    own_generic := new_generic("x"),
    Foo := new_external_class("upstream"),
    method(own_generic, Foo) <- \(x) "from external class"
  )
  # The method is deferred (its signature has an external class), not yet live
  expect_length(methods(downstream$own_generic), 0)

  S7_on_load_(downstream)
  expect_equal(downstream$own_generic(upstream$Foo()), "from external class")
})

test_that("S7_on_unload() unregisters methods dispatching on an external class", {
  upstream <- local_package(
    "upstream_external_class_unload",
    Foo := new_class()
  )
  downstream <- local_package(
    "downstream_external_class_unload",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    own_generic := new_generic("x"),
    Foo := new_external_class(package = "upstream_external_class_unload"),
    method(own_generic, Foo) <- function(x) "from external class"
  )
  downstream$.onLoad()
  expect_equal(downstream$own_generic(upstream$Foo()), "from external class")

  downstream$.onUnload()
  expect_error(
    downstream$own_generic(upstream$Foo()),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() unregisters external-class methods after class unload", {
  generic_pkg <- local_package(
    "upstream_external_unloaded_class_generic",
    gen := new_generic("x")
  )
  class_pkg <- local_package(
    "upstream_external_unloaded_class",
    Foo := new_class()
  )
  downstream <- local_package(
    "downstream_external_unloaded_class",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    gen := new_external_generic(
      package = "upstream_external_unloaded_class_generic",
      dispatch_args = "x"
    ),
    Foo := new_external_class(
      package = "upstream_external_unloaded_class"
    ),
    method(gen, Foo) <- function(x) "from external class"
  )
  downstream$.onLoad()

  obj <- class_pkg$Foo()
  expect_equal(generic_pkg$gen(obj), "from external class")
  expect_equal(nrow(S7_methods(generic = generic_pkg$gen)), 1)

  unloadNamespace("upstream_external_unloaded_class")
  expect_false(isNamespaceLoaded("upstream_external_unloaded_class"))
  expect_equal(generic_pkg$gen(obj), "from external class")

  downstream$.onUnload()
  expect_equal(nrow(S7_methods(generic = generic_pkg$gen)), 0)
})

test_that("method<- NULL removes deferred methods for resolved external classes", {
  upstream <- local_package(
    "upstream_resolved_external_unregister",
    Foo := new_class()
  )
  downstream <- local_package(
    "downstream_resolved_external_unregister",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    own_generic := new_generic("x"),
    Foo := new_external_class(
      package = "upstream_resolved_external_unregister"
    ),
    method(own_generic, Foo) <- function(x) "from external class"
  )
  downstream$.onLoad()
  downstream$ResolvedFoo <- upstream$Foo

  expect_equal(downstream$own_generic(upstream$Foo()), "from external class")
  expect_length(S7_methods_table("downstream_resolved_external_unregister"), 1)

  evalq(method(own_generic, ResolvedFoo) <- NULL, downstream)
  expect_length(S7_methods_table("downstream_resolved_external_unregister"), 0)
  expect_error(
    downstream$own_generic(upstream$Foo()),
    class = "S7_error_method_not_found"
  )

  downstream$.onLoad()
  expect_error(
    downstream$own_generic(upstream$Foo()),
    class = "S7_error_method_not_found"
  )
})

test_that("method<- NULL removes installed hooks for deferred external-class methods", {
  upstream <- local_package(
    "upstream_deferred_external_hook",
    gen := new_generic("x")
  )
  downstream <- local_package(
    "downstream_deferred_external_hook",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    gen := new_external_generic(
      package = "upstream_deferred_external_hook",
      dispatch_args = "x"
    ),
    Ext := new_external_class(
      package = "upstream_deferred_external_hook_class"
    ),
    method(gen, Ext) <- function(x) "from stale hook"
  )
  downstream$.onLoad()
  expect_equal(nrow(S7_methods(generic = upstream$gen)), 0)

  evalq(method(gen, Ext) <- NULL, downstream)
  expect_length(S7_methods_table("downstream_deferred_external_hook"), 0)

  ext_pkg <- local_package(
    "upstream_deferred_external_hook_class",
    Ext := new_class()
  )
  for (hook in package_hooks("upstream_deferred_external_hook_class")) {
    hook()
  }

  expect_equal(nrow(S7_methods(generic = upstream$gen)), 0)
  expect_length(package_hooks("upstream_deferred_external_hook_class"), 0)
  invisible(ext_pkg)
})

test_that("S7_on_unload() unregisters methods and removes hooks", {
  upstream <- local_package("upstream", gen := new_generic("x"))
  downstream <- local_package(
    "downstream",
    Foo := new_class(),
    gen := new_external_generic("upstream", dispatch_args = "x"),
    method(gen, Foo) <- \(x) "dispatched"
  )
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

  downstream <- local_package(
    "downstream_base_ops_unload",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    Foo := new_class(),
    method(`+`, list(Foo, Foo)) <- function(e1, e2) "dispatched"
  )
  downstream$.onLoad()
  expect_equal(downstream$Foo() + downstream$Foo(), "dispatched")

  downstream$.onUnload()
  expect_error(
    downstream$Foo() + downstream$Foo(),
    class = "S7_error_method_not_found"
  )
})

test_that("S7_on_unload() doesn't remove methods registered by another package", {
  upstream <- local_package("upstream_conflict", gen := new_generic("x"))
  first <- local_package(
    "downstream_first",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    gen := new_external_generic("upstream_conflict", dispatch_args = "x"),
    method(gen, class_character) <- function(x) "first"
  )
  first$.onLoad()
  expect_equal(upstream$gen("x"), "first")

  second <- NULL
  expect_message(
    second <- local_package(
      "downstream_second",
      .onLoad <- function(...) S7_on_load(),
      .onUnload <- function(...) S7_on_unload(),
      gen := new_external_generic("upstream_conflict", dispatch_args = "x"),
      method(gen, class_character) <- function(x) "second"
    ),
    "Overwriting method"
  )
  second$.onLoad()
  expect_equal(upstream$gen("x"), "second")

  first$.onUnload()
  expect_equal(upstream$gen("x"), "second")
})

test_that("S7_on_load() removes hooks for deleted external methods", {
  upstream <- local_package("upstream_deleted", gen := new_generic("x"))
  downstream <- local_package(
    "downstream_deleted",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    Foo := new_class(),
    gen := new_external_generic("upstream_deleted", dispatch_args = "x"),
    method(gen, Foo) <- function(x) "dispatched"
  )
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
  downstream <- local_package(
    "downstream_version_gate_unload",
    .onLoad <- function(...) S7_on_load(),
    .onUnload <- function(...) S7_on_unload(),
    gen := new_external_generic(
      "upstream_version_gate_unload",
      dispatch_args = "x",
      version = "1.0.0"
    ),
    method(gen, class_character) <- function(x) "downstream"
  )
  upstream <- local_package(
    "upstream_version_gate_unload",
    gen <- function(x) "not an S7 generic"
  )

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
  ns$keep_ext <- new_external_generic("pkg", "gen", "x")
  ns$drop_me <- generic_sentinel(new_external_generic("pkg", "gen", "x"))

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
