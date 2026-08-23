test_that(":= uses the variable name as `name`", {
  new_thing <- function(name, value = NULL) list(name = name, value = value)

  foo := new_thing()
  expect_equal(foo, list(name = "foo", value = NULL))

  # positional arguments shift to the remaining parameters
  bar := new_thing(1)
  expect_equal(bar, list(name = "bar", value = 1))
})

test_that(":= returns the value invisibly", {
  new_thing <- function(name) list(name = name)
  expect_invisible(foo := new_thing())
})

test_that(":= defers rhs cleanup to the caller, not a transient eval frame", {
  log <- new.env()
  log$cleaned <- FALSE

  record_cleanup <- function(log, name, frame = parent.frame()) {
    defer(log$cleaned <- TRUE, frame = frame)
    name
  }
  outer <- function() {
    thing := record_cleanup(log)
    log$cleaned
  }

  expect_false(outer()) # outer is still running
  expect_true(log$cleaned) # but fires once outer() returns
})

test_that(":= validates its inputs", {
  new_thing <- function(name) list(name = name)
  no_name <- function() "x"

  expect_snapshot(error = TRUE, {
    "foo" := new_thing()
    foo := 10
    foo := new_thing(name = "bar")
    foo := no_name()
  })
})

test_that("S7 := wins search-path conflicts without attach warnings", {
  skip_if(quick_test())

  packages <- c("data.table", "rlang")
  packages <- packages[vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  skip_if(length(packages) == 0, "rlang and data.table are not installed")

  local_dev_S7_lib()

  check_order <- function(package, order) {
    expect_no_error(callr::r(
      function(package, order) {
        messages <- character()
        warnings <- character()

        withCallingHandlers(
          {
            if (identical(order, "S7-first")) {
              library(S7)
              library(package, character.only = TRUE)
            } else {
              library(package, character.only = TRUE)
              library(S7)
            }
          },
          packageStartupMessage = function(cnd) {
            messages <<- c(messages, conditionMessage(cnd))
            invokeRestart("muffleMessage")
          },
          warning = function(cnd) {
            warnings <<- c(warnings, conditionMessage(cnd))
            invokeRestart("muffleWarning")
          }
        )

        stopifnot(exprs = {
          identical(get(":=", mode = "function"), S7::`:=`)
          !any(grepl(":=", messages, fixed = TRUE))
          !any(grepl(":=", warnings, fixed = TRUE))
        })
      },
      args = list(package = package, order = order)
    ))
  }

  for (package in packages) {
    check_order(package, "S7-first")
    check_order(package, "package-first")
  }
})

test_that("loading S7 does not affect := when S7 is not attached", {
  skip_if(quick_test())

  packages <- c("data.table", "rlang")
  packages <- packages[vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  skip_if(length(packages) == 0, "rlang and data.table are not installed")

  local_dev_S7_lib()

  for (package in packages) {
    expect_no_error(callr::r(
      function(package) {
        loadNamespace("S7")
        library(package, character.only = TRUE)
        stopifnot(
          identical(
            get(":=", mode = "function"),
            getExportedValue(package, ":=")
          )
        )
      },
      args = list(package = package)
    ))
  }
})
