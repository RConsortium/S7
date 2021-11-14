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
  expect_equal(foo("blah", 1), "foo-blah-1")

  # t2 has a hard dependency on t0
  library("t0")
  expect_equal(bar("blah", 1), "bar-blah-1")
})
