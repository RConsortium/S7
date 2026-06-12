describe("S7_on_load()", {
  it("doesn't accumulate hooks across repeated loads", {
    external_methods_reset("S7")
    on.exit(external_methods_reset("S7"), add = TRUE)

    Foo <- new_class("Foo")
    Bar <- new_class("Bar")
    external <- new_external_generic("S7", "convert", c("from", "to"))
    external_methods_add(
      "S7",
      external,
      new_signature(list(Foo, Bar)),
      function(from, to, ...) "converted"
    )
    on.exit(
      unregister_S7_method(convert, new_signature(list(Foo, Bar))),
      add = TRUE
    )

    ns <- asNamespace("S7")
    n_hooks <- length(getHook(packageEvent("S7", "onLoad")))

    S7_on_load_(ns)
    S7_on_load_(ns)
    expect_length(getHook(packageEvent("S7", "onLoad")), n_hooks + 1)

    S7_on_unload_(ns)
  })
})

describe("S7_on_unload()", {
  it("unregisters methods and removes hooks", {
    external_methods_reset("S7")
    on.exit(external_methods_reset("S7"), add = TRUE)

    Foo <- new_class("Foo")
    Bar <- new_class("Bar")
    external <- new_external_generic("S7", "convert", c("from", "to"))
    external_methods_add(
      "S7",
      external,
      new_signature(list(Foo, Bar)),
      function(from, to, ...) "converted"
    )
    on.exit(
      unregister_S7_method(convert, new_signature(list(Foo, Bar))),
      add = TRUE
    )

    ns <- asNamespace("S7")
    n_hooks <- length(getHook(packageEvent("S7", "onLoad")))

    S7_on_load_(ns)
    expect_length(getHook(packageEvent("S7", "onLoad")), n_hooks + 1)
    expect_equal(convert(Foo(), Bar), "converted")

    S7_on_unload_(ns)
    expect_length(getHook(packageEvent("S7", "onLoad")), n_hooks)
    expect_null(convert@methods[["Foo"]][["Bar"]])
  })

  it("unregisters methods when a real package is unloaded (#316)", {
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
    expect_length(getHook(packageEvent("t1", "onLoad")), 0)
  })
})

describe("S7_on_build()", {
  it("removes only generic sentinels from the namespace", {
    ns <- new.env(parent = emptyenv())
    ns$keep_fun <- function() {}
    ns$keep_ext <- new_external_generic("pkg", "gen", "x")
    ns$drop_me <- generic_sentinel(new_external_generic("pkg", "gen", "x"))

    strip_generic_sentinels(ns)
    expect_setequal(names(ns), c("keep_ext", "keep_fun"))
  })

  it("is a no-op when there are no sentinels", {
    ns <- new.env(parent = emptyenv())
    ns$x <- 1
    ns$f <- function() {}
    strip_generic_sentinels(ns)
    expect_setequal(names(ns), c("f", "x"))
  })

  it("strips foreign S3 and S4 generics from a real package (#364)", {
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
})
