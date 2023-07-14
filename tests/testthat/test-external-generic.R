test_that("can get and append methods", {
  external_methods_reset("S7")

  expect_equal(external_methods_get("S7"), list())

  bar <- new_external_generic("foo", "bar", "x")
  external_methods_add("S7", bar, list(), function() {})
  expect_equal(
    external_methods_get("S7"),
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
  expect_snapshot({
    print(bar)
  })
})

test_that("new_method works with both hard and soft dependencies", {
  skip_on_os("windows")
  skip_if(quick_test())

  tmp_lib <- tempfile()
  dir.create(tmp_lib)
  old_libpaths <- .libPaths()
  .libPaths(c(tmp_lib, old_libpaths))
  on.exit({
    .libPaths(old_libpaths)
    detach("package:t2", unload = TRUE)
    detach("package:t1", unload = TRUE)
    detach("package:t0", unload = TRUE)
    unlink(tmp_lib, recursive = TRUE)
  })

  quick_install(test_path(c("t0", "t1", "t2")))
  library("t2")

  # t2 has a soft dependency on t1
  library("t1")
  expect_equal(foo("x"), "foo")

  # t2 has a hard dependency on t0
  library("t0")
  expect_equal(bar("x"), "bar")
})
