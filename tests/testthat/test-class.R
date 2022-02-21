describe("R7 classes", {
  it("possess expected properties", {
    foo <- new_class("foo", validator = function(self) NULL)

    expect_equal(prop_names(foo), setdiff(names(attributes(foo)), "class"))
    expect_type(foo@name, "character")
    expect_equal(foo@parent, R7_object)
    expect_type(foo@constructor, "closure")
    expect_type(foo@validator, "closure")
    expect_type(foo@properties, "list")
  })

  it("print nicely", {
    foo1 <- new_class("foo1", properties = list(x = "integer", y = "integer"))
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

      new_class("foo", constructor = 1)
      new_class("foo", constructor = function() {})
    })
  })

  it("can't inherit from S4 or class unions", {
    parentS4 <- methods::setClass("parentS4", slots = c(x = "numeric"))
    expect_snapshot(error = TRUE, {
      new_class("test", parent = parentS4)
      new_class("test", parent = new_union("character"))
    })
  })
})

describe("inheritance", {
  it("combines properties for parent classes", {
    foo1 <- new_class("foo1", properties = list(x = double))
    foo2 <- new_class("foo2", foo1, properties = list(y = double))
    expect_equal(names(foo2@properties), c("x", "y"))
  })
  it("child properties override parent", {
    foo1 <- new_class("foo1", properties = list(x = "numeric"))
    foo2 <- new_class("foo2", foo1, properties = list(x = double))
    expect_equal(names(foo2@properties), "x")
    expect_equal(foo2@properties$x$class, base_classes$double)
  })
})

describe("new_object()", {
  it("gives useful error if called directly",{
    expect_snapshot(new_object(), error = TRUE)
  })

  it("validates object", {
    foo <- new_class("foo",
      properties = list(new_property("x", double)),
      validator = function(self) if (self@x < 0) "x must be positive"
    )

    expect_snapshot(error = TRUE, {
      foo("x")
      foo(-1)
    })
  })
})

describe("R7 object", {
  it("has an R7 and S3 class", {
    foo <- new_class("foo")
    x <- foo()
    expect_equal(object_class(x), foo)
    expect_equal(class(x), c("foo", "R7_object"))
  })

  it("displays nicely", {
    expect_snapshot({
      foo <- new_class("foo", properties = list(x = double, y = double))
      foo()
      str(list(foo()))
    })
  })

  it("displays objects with data nicely", {
    expect_snapshot({
      text <- new_class("text", character)
      text("x")
      str(list(text("x")))
    })
  })
})

describe("default constructor", {
  it("initializes properties with defaults", {
    foo1 <- new_class("foo1", properties = list(x = double))
    expect_equal(props(foo1()), list(x = double()))

    foo2 <- new_class("foo2", foo1, properties = list(y = double))
    expect_equal(props(foo2()), list(x = double(), y = double()))
  })

  it("overrides properties with arguments", {
    foo1 <- new_class("foo1", properties = list(x = double))
    foo2 <- new_class("foo2", foo1, properties = list(y = double))
    expect_equal(props(foo2(x = 1)), list(x = 1, y = double()))
    expect_equal(props(foo2(x = 1, y = 2)), list(x = 1, y = 2))
  })

  it("can initialise a property to NULL", {
    foo <- new_class("foo", properties = list(
      new_property("x", default = 10)
    ))
    x <- foo(x = NULL)
    expect_equal(x@x, NULL)
  })

  it("initializes data with defaults", {
    text1 <- new_class("text1", parent = "character")
    obj <- text1()
    expect_equal(R7_data(obj), character())
  })

  it("overrides data with defaults", {
    text1 <- new_class("text1", parent = "character")
    expect_equal(R7_data(text1("x")), "x")
  })

  it("initializes property with R7 object", {
    foo1 <- new_class("foo1")
    foo2 <- new_class("foo2", properties = list(x = foo1))
    x <- foo2()
    expect_s3_class(x@x, "foo1")
  })
})
