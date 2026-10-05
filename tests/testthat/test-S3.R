test_that("new_S3_class has a print method", {
  expect_snapshot(new_S3_class(c("ordered", "factor")))
})

test_that("can construct objects that extend S3 classes", {
  ordered2 := new_class(parent = class_factor, package = NULL)
  x <- ordered2(c(1L, 2L, 1L), letters[1:3])
  expect_equal(class(x), c("ordered2", "factor", "S7_object"))
  expect_equal(prop_names(x), character())
  expect_error(x@levels, "Property not found")
})

test_that("subclasses inherit validator", {
  foo <- new_S3_class(
    "foo",
    function(.data) structure(.data, class = "foo"),
    function(x) if (!is.double(x)) "Underlying data must be a double"
  )
  foo2 := new_class(foo, package = NULL)

  expect_snapshot(error = TRUE, foo2("a"))
})

test_that("S3 declarations control inherited attributes during construction (#760)", {
  labelled <- new_S3_class(
    "labelled",
    constructor = function(.data = double(), label = "") {
      structure(.data, label = label, class = "labelled")
    },
    attributes = c("names", "label")
  )
  Base := new_class(parent = labelled, properties = list(x = class_double))
  Left := new_class(parent = Base, properties = list(left = class_double))
  Right := new_class(
    parent = Base,
    properties = list(right = class_double),
    constructor = function(parent, right = 0) new_object(parent, right = right)
  )
  parent <- Left(.data = c(a = 1), label = "kept", x = 2, left = 3)
  attr(parent, "foreign") <- TRUE

  object <- Right(parent = parent, right = 4)
  expect_identical(
    S7_data(object),
    structure(c(a = 1), label = "kept", class = "labelled")
  )
  expect_identical(props(object), list(x = 2, right = 4))
  expect_null(attr(object, "left", exact = TRUE))
  expect_null(attr(object, "foreign", exact = TRUE))
  expect_identical(parent@left, 3)

  dirty <- structure(c(a = 1), foreign = TRUE)
  expect_null(attr(Base(.data = dirty), "foreign", exact = TRUE))
})

test_that("S3 attribute declarations distinguish unknown and empty sets (#760)", {
  for (attributes in list(NULL, character())) {
    plain <- new_S3_class(
      "plain",
      constructor = function(.data = double()) {
        structure(.data, class = "plain")
      },
      attributes = attributes
    )
    Child := new_class(parent = plain)
    object <- Child(.data = structure(1, foreign = TRUE))
    expect_identical(
      attr(object, "foreign", exact = TRUE),
      if (is.null(attributes)) TRUE
    )
    expect_identical(class(S7_data(object)), "plain")
  }
})

test_that("S3 attribute declarations work through external parents (#760)", {
  pkg := local_package({
    labelled <- new_S3_class(
      "labelled",
      constructor = function(.data = double(), label = "") {
        structure(.data, label = label, class = "labelled")
      },
      attributes = "label"
    )
    Base := new_class(parent = labelled, properties = list(x = class_double))
  })
  Base := new_external_class(package = "pkg")
  Child := new_class(parent = Base)
  object <- Child(.data = structure(1, foreign = TRUE), label = "kept", x = 2)
  expect_identical(object@x, 2)
  expect_identical(attr(object, "label", exact = TRUE), "kept")
  expect_null(attr(object, "foreign", exact = TRUE))
})

test_that("bundled S3 wrappers preserve their data attributes (#760)", {
  cases <- list(
    list(class = class_factor, data = factor(c(a = "x"))),
    list(class = class_Date, data = .Date(c(a = 1))),
    list(class = class_POSIXct, data = .POSIXct(c(a = 1), tz = "UTC")),
    list(class = class_POSIXlt, data = as.POSIXlt(.POSIXct(1, tz = "UTC"))),
    list(class = class_data.frame, data = data.frame(x = 1, row.names = "a")),
    list(class = class_formula, data = y ~ x)
  )
  for (case in cases) {
    Child := new_class(
      parent = case$class,
      constructor = function(parent) new_object(parent)
    )
    parent <- case$data
    attr(parent, "foreign") <- TRUE
    expect_identical(S7_data(Child(parent = parent)), case$data)
  }
})

test_that("new_S3_class() checks attribute names", {
  expect_snapshot(error = TRUE, new_S3_class("foo", attributes = 1))
  expect_snapshot(error = TRUE, new_S3_class("foo", attributes = NA_character_))
  expect_snapshot(error = TRUE, new_S3_class("foo", attributes = ""))
})


test_that("new_S3_class() checks its inputs", {
  expect_snapshot(new_S3_class(1), error = TRUE)

  expect_snapshot(error = TRUE, {
    new_S3_class("foo", function(x) {})
    new_S3_class("foo", function(.data, ...) {})
  })

  expect_snapshot(
    new_S3_class("data.frame", default = data.frame()),
    error = TRUE
  )
})

test_that("new_S3_class() defaults are evaluated for each property construction", {
  defaults <- constructors <- 0L
  make_default <- function() {
    defaults <<- defaults + 1L
    data.frame(x = defaults)
  }
  custom <- new_S3_class(
    "data.frame",
    constructor = function(.data = list()) {
      constructors <<- constructors + 1L
      list2DF(.data)
    },
    default = quote(make_default())
  )
  Survey := new_class(properties = list(survey = custom))
  expect_identical(formals(Survey)$survey, quote(make_default()))
  expect_identical(defaults, 0L)
  expect_identical(constructors, 0L)
  expect_identical(Survey()@survey, data.frame(x = 1L))
  expect_identical(Survey()@survey, data.frame(x = 2L))
  expect_identical(defaults, 2L)
  expect_identical(constructors, 0L)

  Override := new_class(
    properties = list(
      survey = new_property(custom, default = quote(data.frame(x = 10L)))
    )
  )
  expect_identical(Override()@survey, data.frame(x = 10L))
  expect_identical(defaults, 2L)
})

test_that("new_S3_class() can supply a property default without a constructor", {
  epoch <- .Date(0)
  Date <- new_S3_class("Date", default = quote(epoch))
  Event := new_class(properties = list(date = Date))
  expect_identical(formals(Event)$date, quote(epoch))
  expect_identical(Event()@date, epoch)
})


test_that("default new_S3_class constructor errors", {
  # constructor errors if needed
  expect_snapshot(class_construct(new_S3_class("foo"), 1), error = TRUE)
})

test_that("can construct data frame subclass", {
  dataframe2 := new_class(class_data.frame)
  df <- dataframe2(list(x = 1:3))
  expect_s3_class(df, "data.frame")
})

test_that("inherits() works with S7_S3_class", {
  skip_unless_r("> 4.3.0")

  expect_true(inherits(factor("a"), class_factor))
  expect_false(inherits(1, class_factor))
  expect_true(inherits(Sys.Date(), new_S3_class("Date")))
})

# Basic tests of validators -----------------------------------------------

test_that("catches invalid factors", {
  expect_snapshot({
    validate_factor(structure("x"))
  })
})

test_that("catches invalid dates", {
  expect_snapshot({
    validate_date("x")
  })
})

test_that("catches invalid POSIXct", {
  expect_snapshot({
    validate_POSIXct(structure("x", tzone = "UTC"))
    validate_POSIXct(structure(1, tzone = 1))
  })
  expect_null(validate_POSIXct(Sys.time()))
})

test_that("catches invalid data.frame", {
  expect_snapshot({
    validate_data.frame(1)
    validate_data.frame(structure(list(x = 1, y = 1:2), row.names = 1L))
    validate_data.frame(structure(list(x = 1, y = 1), row.names = 1:2))
    validate_data.frame(structure(list(1), row.names = 1L))
    validate_data.frame(structure(
      list(y = 1:2, x = data.frame(x1 = 1:3)),
      row.names = 1:2
    ))
  })
})

test_that("data.frame accepts data.frame and matrix columns (#751)", {
  packed <- data.frame(y = 1:3)
  packed$x <- data.frame(x1 = 1:3, x2 = 4:6)
  expect_null(validate_data.frame(packed))

  mat <- data.frame(y = 1:3)
  mat$m <- matrix(1:6, nrow = 3)
  expect_null(validate_data.frame(mat))
})
