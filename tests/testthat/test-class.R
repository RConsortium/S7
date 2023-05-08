describe("S7 classes", {
  it("possess expected properties", {
    foo <- new_class("foo", package = "S7", validator = function(self) NULL)

    expect_equal(prop_names(foo), setdiff(names(attributes(foo)), "class"))
    expect_type(foo@name, "character")
    expect_equal(foo@parent, S7_object)
    expect_type(foo@constructor, "closure")
    expect_type(foo@validator, "closure")
    expect_type(foo@properties, "list")
  })

  it("print nicely", {
    foo1 <- new_class("foo1", properties = list(x = class_integer, y = class_integer))
    foo2 <- new_class("foo2", foo1)

    expect_snapshot({
      foo2

      str(foo2)
      # Omit details when nested
      str(list(foo2))
    })
  })

  it("checks inputs", {
    expect_snapshot(error = TRUE, {
      new_class(1)
      new_class("foo", 1)

      new_class("foo", package = 1)

      new_class("foo", constructor = 1)
      new_class("foo", constructor = function() {})

      new_class("foo", validator = function() {})
    })
  })

  it("can't inherit from S4 or class unions", {
    parentS4 <- methods::setClass("parentS4", slots = c(x = "numeric"))
    expect_snapshot(error = TRUE, {
      new_class("test", parent = parentS4)
      new_class("test", parent = new_union("character"))
    })
  })

  it("can't inherit from an environment", {
    expect_snapshot(error = TRUE, {
      new_class("test", parent = class_environment)
    })
  })
})

describe("inheritance", {
  it("combines properties for parent classes", {
    foo1 <- new_class("foo1", properties = list(x = class_double))
    foo2 <- new_class("foo2", foo1, properties = list(y = class_double))
    expect_equal(names(foo2@properties), c("x", "y"))
  })
  it("child properties override parent", {
    foo1 <- new_class("foo1", properties = list(x = class_numeric))
    foo2 <- new_class("foo2", foo1, properties = list(x = class_double))
    expect_equal(names(foo2@properties), "x")
    expect_equal(foo2@properties$x$class, class_double)
  })
})

describe("abstract classes", {
  it("can't be instantiated", {
    expect_snapshot(error = TRUE, {
      foo <- new_class("foo", abstract = TRUE)
      foo()
    })
  })
  it("can't inherit from concrete class", {
    expect_snapshot(error = TRUE, {
      foo1 <- new_class("foo1")
      new_class("foo2", parent = foo1, abstract = TRUE)
    })
  })
  it("can construct concrete subclasses", {
    foo1 <- new_class("foo1", abstract = TRUE)
    foo2 <- new_class("foo2", parent = foo1)
    expect_s3_class(foo2(), "foo2")
  })
})

describe("new_object()", {
  it("gives useful error if called directly",{
    expect_snapshot(new_object(), error = TRUE)
  })

  it("validates object", {
    foo <- new_class("foo",
      properties = list(x = new_property(class_double)),
      validator = function(self) if (self@x < 0) "x must be positive"
    )

    expect_snapshot(error = TRUE, {
      foo("x")
      foo(-1)
    })
  })

  it("runs each parent validator exactly once", {
    A <- new_class("A", validator = function(self) cat("A "))
    B <- new_class("B", parent = A, validator = function(self) cat("B "))
    C <- new_class("C", parent = B, validator = function(self) cat("C "))

    expect_snapshot({
      . <- A()
      . <- B()
      . <- C()
    })
  })
})

describe("S7 object", {
  it("has an S7 and S3 class", {
    foo <- new_class("foo")
    x <- foo()
    expect_equal(S7_class(x), foo)
    expect_equal(class(x), c("foo", "S7_object"))
  })

  it("displays nicely", {
    expect_snapshot({
      foo <- new_class("foo", properties = list(x = class_double, y = class_double))
      foo()
      str(list(foo()))
    })
  })

  it("displays objects with data nicely", {
    expect_snapshot({
      text <- new_class("text", class_character)
      text("x")
      str(list(text("x")))
    })
  })

  it("displays list objects nicely", {
    foo1 <- new_class(
      "foo1",
      parent = class_list,
      properties = list(x = class_double, y = class_list)
    )
    expect_snapshot(
      foo1(
        list(
          x = 1,
          y = list(a = 21, b = 22)
        ),
        x = 3,
        y = list(a = 41, b = 42)
      )
    )
  })
})

describe("default constructor", {
  it("initializes properties with defaults", {
    foo1 <- new_class("foo1", properties = list(x = class_double))
    expect_equal(props(foo1()), list(x = double()))

    foo2 <- new_class("foo2", foo1, properties = list(y = class_double))
    expect_equal(props(foo2()), list(x = double(), y = double()))
  })

  it("overrides properties with arguments", {
    foo1 <- new_class("foo1", properties = list(x = class_double))
    foo2 <- new_class("foo2", foo1, properties = list(y = class_double))
    expect_equal(props(foo2(x = 1)), list(x = 1, y = double()))
    expect_equal(props(foo2(x = 1, y = 2)), list(x = 1, y = 2))
  })

  it("can initialise a property to NULL", {
    foo <- new_class("foo", properties = list(
      x = new_property(default = 10)
    ))
    x <- foo(x = NULL)
    expect_equal(x@x, NULL)
  })

  it("initializes data with defaults", {
    text1 <- new_class("text1", parent = class_character)
    obj <- text1()
    expect_equal(S7_data(obj), character())
  })

  it("overrides data with defaults", {
    text1 <- new_class("text1", parent = class_character)
    expect_equal(S7_data(text1("x")), "x")
  })

  it("initializes property with S7 object", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", properties = list(x = foo1))
    x <- foo2()
    expect_s3_class(x@x, "foo1")
  })
})

test_that("c(<S7_class>, ...) gives error", {
  foo1 <- new_class("foo1")
  expect_snapshot(error = TRUE, {
    c(foo1, foo1)
  })
})
