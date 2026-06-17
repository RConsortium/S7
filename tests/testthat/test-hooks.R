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
