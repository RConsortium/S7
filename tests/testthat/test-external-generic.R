test_that("can get and append methods", {
  external_methods_reset("S7")
  on.exit(external_methods_reset("S7"), add = TRUE)

  expect_equal(S7_methods_table("S7"), list())

  bar <- new_external_generic("foo", "bar", "x")
  external_methods_add("S7", bar, list(), function() {})
  expect_equal(
    S7_methods_table("S7"),
    list(
      list(
        generic = bar,
        signature = list(),
        method = function() {}
      )
    )
  )
})

test_that("displays nicely", {
  bar <- new_external_generic("foo", "bar", "x")
  on.exit(external_methods_reset("S7"), add = TRUE)

  expect_snapshot({
    print(bar)
  })
})

test_that("can convert existing generics to external", {
  foo_S7 <- new_generic("foo_S7", "x")
  env <- new.env()
  env$.packageName <- "test"
  environment(foo_S7) <- env

  expect_equal(
    as_external_generic(foo_S7),
    new_external_generic("test", "foo_S7", "x")
  )

  foo_ext <- new_external_generic("pkg", "foo", "x")
  expect_equal(as_external_generic(foo_ext), foo_ext)

  expect_equal(
    as_external_generic(as_S3_generic(sum)),
    new_external_generic("base", "sum", "__S3__")
  )

  methods::setGeneric("foo_S4", function(x) {})
  expect_equal(
    as_external_generic(foo_S4),
    new_external_generic("S7", "foo_S4", "x")
  )
})

test_that("new_method works with both hard and soft dependencies", {
  # NB: Relies on installed S7

  skip_if(getRversion() < "4.1" && Sys.info()[["sysname"]] == "Windows")
  skip_if(quick_test())


  on.exit({
    .libPaths(old_libpaths)
    try(detach("package:t2", unload = TRUE), silent = TRUE)
    try(detach("package:t1", unload = TRUE), silent = TRUE)
    try(detach("package:t0", unload = TRUE), silent = TRUE)
    unlink(tmp_lib, recursive = TRUE)
    # remove.packages(c("t1", "t0", "t2"))
  })

  tmp_lib <- tempfile()
  dir.create(tmp_lib)
  old_libpaths <- .libPaths()
  .libPaths(c(tmp_lib, old_libpaths))

  # t2 has a hard dependency on t0
  # t2 has a soft dependency on t1

  # First, ensure that t2 can install and run successfully without t1 installed
  quick_install(test_path("t0"), tmp_lib)
  quick_install(test_path("t2"), tmp_lib)

  library("t2")
  library("t0")
  expect_equal(an_s3_generic(t2::an_s7_class()), "foo")
  expect_equal(an_s7_generic("x"), "foo")

  # Now install the soft dependency
  quick_install(test_path("t1"), tmp_lib)

  library("t1")
  expect_equal(another_s3_generic(t2::an_s7_class()), "foo")
  expect_equal(another_s7_generic("x"), "foo")


  ## Check again in a fresh session, with everything installed
  expect_no_error(callr::r(function() {
    library(t2)

    stopifnot(exprs = {
      t0::an_s3_generic(an_s7_class()) == "foo"
      t0::an_s7_generic("x") == "foo"
    })

    if(isNamespaceLoaded("t1"))
      stop("Prematurely loaded {t1}")

    stopifnot(exprs = {
      t1::another_s3_generic(an_s7_class()) == "foo"
      t1::another_s7_generic("x") == "foo"
    })

    NULL
  }))

})
