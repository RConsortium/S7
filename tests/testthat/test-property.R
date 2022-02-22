describe("property retrieval", {
  it("retrieves the properties that exist & errors otherwise", {
    foo <- new_class("foo", properties = list(xyz = double))
    obj <- foo(1)
    expect_equal(prop(obj, "xyz"), 1)
    expect_equal(obj@xyz, 1)

    expect_snapshot_error(prop(obj, "x"))
    expect_snapshot_error(obj@x)
  })
  it("evalutes dynamic properties", {
    foo <- new_class("foo", properties = list(
      new_property("x", getter = function(self) 1)
    ))
    obj <- foo()
    expect_equal(prop(obj, "x"), 1)
    expect_equal(obj@x, 1)
  })

  it("falls back to `base::@` for non-R7 objects", {
    expect_snapshot(error = TRUE, {
      "foo"@blah
      NULL@blah
    })
  })
})

describe("prop setting", {
  it("can set a property", {
    foo <- new_class("foo", properties = list(xyz = double))
    obj <- foo(1)

    prop(obj, "xyz") <- 2
    expect_equal(obj@xyz, 2)

    obj@xyz <- 3
    expect_equal(obj@xyz, 3)
  })

  it("can set dynamic properties", {
    foo <- new_class("foo", properties = list(
      new_property("x", setter = function(self, value) {
        self@x <- value * 2
        self
      })
    ))
    obj <- foo()
    obj@x <- 1
    expect_equal(obj@x, 2)
  })

  it("errors if the property doesn't exist or is wrong class", {
    foo <- new_class("foo", properties = list(x = double))
    obj <- foo(123)
    expect_snapshot(error = TRUE, {
      x@foo <- 10
      x@x <- "x"
    })
  })

  it("does not run the check or validation functions if check = FALSE", {
    foo <- new_class("foo", properties = list(x = double))
    obj <- foo(123)
    prop(obj, "x", check = FALSE) <- "foo"
    expect_equal(obj@x, "foo")
  })

  it("falls back to `base::@` for non-R7 objects", {
    x <- "foo"
    expect_error(x@blah <- "bar", "is not a slot in class")
  })
})

describe("props<-", {
  it("validates after setting all properties", {
    foo <- new_class("foo",
      properties = list(x = double, y = double),
      validator = function(self) if (self@x > self@y) "bad"
    )

    obj <- foo(1, 2)
    props(obj) <- list(x = 5, y = 10)
    expect_equal(obj@x, 5)
    expect_equal(obj@y, 10)
  })
})

describe("property access", {
  it("access en masse", {
    foo <- new_class("foo", properties = list(x = "numeric", y = "numeric"))
    x <- foo(x = 1, y = 2)
    expect_equal(prop_names(x), c("x", "y"))
    expect_equal(props(x), list(x = 1, y = 2))
    expect_true(prop_exists(x, "x"))
    expect_true(prop_exists(x, "y"))
    expect_false(prop_exists(x, "z"))
  })

  it("can access dynamic properties", {
    foo <- new_class("foo", properties = list(
      new_property("x", getter = function(self) 10),
      new_property("y")
    ))
    x <- foo(y = 2)
    expect_equal(props(x), list(x = 10, y = 2))
  })

  it("can with property-less object", {
    x <- new_class("x")()
    expect_equal(prop_names(x), character())
    expect_equal(props(x), list())
    expect_equal(prop_exists(x, "y"), FALSE)
  })

  it("ignore attributes that are not properties", {
    x <- new_class("x")()
    attr(x, "extra") <- 1

    expect_equal(prop_names(x), character())
    expect_equal(props(x), list())
    expect_false(prop_exists(x, "extra"))
  })
})


test_that("properties can be NULL", {
  foo <- new_class("foo", properties = list(x = any_class))
  x <- foo(x = NULL)
  expect_equal(x@x, NULL)
  x@x <- 1
  expect_equal(x@x, 1)
  x@x <- NULL
  expect_equal(x@x, NULL)
  expect_equal(prop_names(x), "x")
  expect_equal(props(x), list(x = NULL))
})

describe("new_property()", {
  it("validates name", {
    expect_snapshot(error = TRUE, {
      new_property(1)
      new_property("")
    })
  })

  it("validates getter and settor", {
    expect_snapshot(error = TRUE, {
      new_property("x", getter = function(x) {})
      new_property("x", setter = function(x, y, z) {})
    })
  })

  it("validates default", {
    expect_snapshot(error = TRUE, {
      new_property("foo", class = "integer", default = "x")
    })
  })

  it("displays nicely", {
    x <- new_property("foo", "integer")
    expect_snapshot({
      print(x)
      str(list(x))
    })
  })
})

test_that("properties can be base, S3, S4, R7, or R7 union", {
  class_R7 <- new_class("class_R7")
  class_S4 <- methods::setClass("class_S4", slots = c(x = "numeric"))

  my_class <- new_class("my_class",
    properties = list(
      anything = any_class,
      null = NULL,
      base = "integer",
      S3 = new_S3_class("factor"),
      S4 = class_S4,
      R7 = class_R7,
      R7_union = new_union("integer", "logical")
    )
  )
  expect_snapshot(my_class)
  my_obj <- my_class(
    anything = TRUE,
    null = NULL,
    base = 1L,
    S3 = factor(),
    S4 = class_S4(x = 1),
    R7 = class_R7(),
    R7_union = 1L
  )

  # First check that we can set with out error
  expect_error(my_obj@base <- 2L, NA)
  expect_error(my_obj@S3 <- factor("x"), NA)
  expect_error(my_obj@S4 <- class_S4(x = 2), NA)
  expect_error(my_obj@R7 <- class_R7(), NA)
  expect_error(my_obj@R7_union <- 2L, NA)
  expect_error(my_obj@R7_union <- TRUE, NA)

  # Then capture the error messages for human inspection
  expect_snapshot(error = TRUE, {
    my_obj@null <- "x"
    my_obj@base <- "x"
    my_obj@S3 <- "x"
    my_obj@S4 <- "x"
    my_obj@R7 <- "x"
    my_obj@R7_union <- "x"
  })
})

test_that("as_properties normalises properties", {
  expect_equal(as_properties(NULL), list())
  expect_equal(
    as_properties(list(new_property("y"))),
    list(y = new_property("y")
  ))
  expect_equal(
    as_properties(list(x = "numeric")),
    list(x = new_property("x", "numeric")
  ))
})

test_that("as_properties() gives useful error messages", {
  expect_snapshot(error = TRUE, {
    as_properties(1)
    as_properties(list(1))
    as_properties(list(x = 1))
    as_properties(list(x = "character", x = "character"))
  })
})
